static char *sccs = "@(#)ims_stragglers.c	1.1  06/27/97";

/***********************************************************
**
** File:        ims_stragglers.c
**
** Function:    This application will search through the directories for
**              given datasets and determine if any files do not
**              correspond to a granules entry.  it is assumed that
**              a directory cannot belong to more that one dataset_idx.
**
**
**
** Author:      David Pass
**
** Date:        6/18/97
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
#include <ims_timeConv.h>
#include <ims_getInput.h>
#include <ims_childHandler.h>
#include <unistd.h>

/*
** Local Definitions.
*/

#define  MAX_FNAMES  100      /*  buffer size for file names  */
#define  MAX_FNAME_LEN  64    /* max size for names in directory */

/*
** Log File Information structure definition.
*/
typedef struct logInfo{
    int  logFlag;
    char *path;
    char *fileName;
} LOG_SPEC;

/*
** User Information structure definition.
*/
typedef struct userSpec{
    char *username;
    char *password;
    char *server;
    char *database;
    char *program;
} USER_SPEC;


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
** Granule Information Structure Definition.
*/
typedef  struct  granule_data__t *pnt_granule_data_t;
typedef struct granule_data__t{
    int    granule_idx;
    short  status;
    char   received_time[31];
    char   contributor[31];
    short  version;
    char   name[31];
    char   ims_visible_p[2];
} granule_data_t;


/*
** Dataset List Information Structure Definition.
*/

typedef  struct  dataset_list__t *pnt_dataset_list_t;
typedef struct dataset_list__t{
    short  dataset_idx;
    char   granules_table[IMS_COL30_LEN+1];
    char   version_p; /* if yes, version put in path  */
    short  int process_level;
    short  oagdr; /* to check if dataset is orderable  */
    char   mfd_p[2]; /* marked for delete: if Y, then data file
            only deleted if granule is deleted. (.M file and
            granule entry not deleted.)  */
    pnt_path_list_t     pnt_path_list;
    pnt_ext_list_t      pnt_ext_list;
    pnt_dataset_list_t  pnt_next;
} dataset_list_t;


/*
** file name buffers: so not alot of allocates.
*/
typedef  struct  file_names__t *pnt_file_names_t;
typedef struct file_names__t{
    char name[MAX_FNAMES][MAX_FNAME_LEN+1];
    pnt_file_names_t pnt_next;
} file_names_t;



/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
    char *username;
    char *password;
    char *logFilePath;
    char *logFileName;
    char *startDataset; /* first dataset to process (null=0) */
    char *endDataset; /* last dataset to process (null=inf) */
    char *commandFile;
    char *server;
    char *database;
    char *logFlag;
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
    {"-L",              &commands.logFilePath},
    {"+logFilePath",    &commands.logFilePath},
    {"-N",              &commands.logFileName},
    {"+logFileName",    &commands.logFileName},
    {"-S",              &commands.startDataset},
    {"+startDataset",   &commands.startDataset},
    {"-E",              &commands.endDataset},
    {"+endDataset",     &commands.endDataset},
    {"-C",              &commands.commandFile},
    {"+commandFile",    &commands.commandFile},
    {"-X",              &commands.server},
    {"+server",         &commands.server},
    {"-Y",              &commands.database},
    {"+database",       &commands.database},
    {"-l",              &commands.logFlag},
    {"+logFlag",        &commands.logFlag},
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
    {"logFilePath",     &commands.logFilePath},
    {"logFileName",     &commands.logFileName},
    {"startDataset",    &commands.startDataset},
    {"endDataset",      &commands.endDataset},
    {"server",          &commands.server},
    {"database",        &commands.database},
    {"logFlag",         &commands.logFlag}

};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;


/*
** Local Functions
*/
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, USER_SPEC *, LOG_SPEC *);
static int cacheDatasetInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    dataset_list_t **);
static int closeConnection (IMS_QI_DESC_OBJ *);
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **,
    USER_SPEC *);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int go_through_datasets(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    dataset_list_t *, long, long );
static int freeCacheData(IMS_MSG_STRUCT *, dataset_list_t *);
static  int get_path_info ( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    long,  pnt_dataset_list_t  );
static  int get_file_info ( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    long,  pnt_dataset_list_t  );
int get_items_value( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
    char * , char[10][60] );
static int checkRetStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);


/***************************************************************
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
    USER_SPEC userSpec;
    char logFileSpec[IMS_PATH_LEN+1];
    char writeBuffer[100];
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    int fd;
    IMS_QI_DESC_OBJ *qDesc;
    pnt_dataset_list_t pnt_dataset_list;
    char  gran_status[10][60]; /* this is the status meanings
            as obtained from the table items.  */
    char  process_status[10][60];
    long  i,j;
    short  check_files;
    long  num_items_total;
    long  time_delay;


    commands.username = NULL;
    commands.password = NULL;
    commands.logFilePath = NULL;
    commands.logFileName = NULL;
    commands.startDataset = NULL;
    commands.endDataset = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.logFlag = NULL;
    commands.help = NULL;
    commands.release = NULL;

    /*
    ** Initialize variables.
    */
    (void) memset (&logInfo, 0, (size_t) sizeof (LOG_SPEC));
    (void) memset (&userSpec, 0, (size_t) sizeof (USER_SPEC));

    /*
    ** Get the program name and the node name.
    */
    programName = ims_extractFileName (argv[0]);
    userSpec.program = programName;
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

    /*
    ** Get the command-line arguments. The variable status will
    ** actually contain the number of command-line arguments
    ** processed upon successful completion.
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
        ** Now, get command-line arguments again to overlay file args.
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
    if ((status = getArgInput (msgDesc, &userSpec, &logInfo)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Set-up the log file if requested.
    */
    if (logInfo.logFlag == IMS_TRUE){
        /*
        ** Assemble the log file specification.
        */
        ims_concatFilePath( logFileSpec, logInfo.path,logInfo.fileName);

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
            "\n\n>>>>>>>>>>>>>>>>>>  IMS STRAGGLERS STARTUP  "
            "<<<<<<<<<<<<<<<<<<\n\n");
        if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not write to log file '%s'. %s",
                logFileSpec, strerror (errno));
            goto ERROR;
        }
    }

    /*
    ** Okay, go do the work...
    */

    if (openConnection(msgDesc, &qDesc, &userSpec) < IMS_OK){
        goto ERROR;
    }

    /*
    ** Cache the policy dataset, path, and file information.
    */
    if (cacheDatasetInfo(msgDesc, qDesc, &pnt_dataset_list ) < IMS_OK){
        closeConnection(qDesc);
        goto ERROR;
    }

    i = atol( commands.startDataset );
    j = atol( commands.endDataset );
    if(  j  <=  0  )  j = 300000;  /* if -1 or 0, do all  */

    if( go_through_datasets(msgDesc, qDesc, pnt_dataset_list, i,
        j )  < IMS_OK){
        closeConnection(qDesc);
        /*
        ** Dump the dataset information.
        */
        (void) freeCacheData(msgDesc, pnt_dataset_list);
        goto ERROR;
    }

    /*
    ** Dump the dataset information.
    */
    (void) freeCacheData(msgDesc, pnt_dataset_list);
    closeConnection(qDesc);

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The ims_stragglers processing is complete.");

    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    (void) ims_msgStderrFlag (msgDesc, IMS_ON);
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_stragglers processing failed.");

    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);
    exit (1);
}   /* main */


/***************************************************************
**
** runDown ()
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
** usage ()
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
** getArgInput ()
**
** Process command-line and command-file arguments.
**
**************************************************************** */

static int getArgInput (
    IMS_MSG_STRUCT *msgDesc,
    USER_SPEC *userSpec,
    LOG_SPEC *logInfo)
{
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    long  i,j;


    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username != (char *) NULL)
    {
        userSpec->username = commands.username;
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

        userSpec->username = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec->username, inputBuffer);
    }

    /* password */
    if (commands.password != (char *) NULL)
    {
        userSpec->password = commands.password;
    }
    else
    {
        if (ims_getPassword (inputBuffer) == NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        userSpec->password = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec->password, inputBuffer);
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
        logInfo->fileName = malloc( (size_t)9 + IMS_PROGRAM_LEN + 11);
        (void) sprintf (logInfo->fileName, "errorlog_%s%s",
            programName, ims_timeStamp ());
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

    try_again:;
    /* startDataset  */
    if (commands.startDataset  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "starting dataset number: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer );
        for( j=0; j  <  i ; j++ ){
            if(  !isdigit( inputBuffer[j] ) &&
                inputBuffer[j]  !=  '+'  &&  inputBuffer[j]  !=  '-' ){
                (void) ims_msg (msgDesc, IMS_INFO,
                    "startDataset must be a number: try again." );
                goto try_again;
            }
        }
        commands.startDataset = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.startDataset, inputBuffer);
    }

    try_again2:;
    /* endDataset  */
    if (commands.endDataset  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "ending dataset number: ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer );
        for( j=0; j  <  i ; j++ ){
            if(  !isdigit( inputBuffer[j] ) &&
                inputBuffer[j]  !=  '+'  &&  inputBuffer[j]  !=  '-' ){
                (void) ims_msg (msgDesc, IMS_INFO,
                    "endDataset must be a number: try again." );
                goto try_again;
            }
        }
        commands.endDataset = (char *) malloc( i * sizeof( char ));
        (void) strcpy (commands.endDataset, inputBuffer);
    }

    /* server */
    if (commands.server != (char *) NULL){
        userSpec->server = commands.server;
    }

    /* database */
    if (commands.database != (char *) NULL){
        userSpec->database = commands.database;
    }

    return (IMS_OK);
}   /* getArgInput */


/*******************************************************************
**
** cacheDatasetInfo
**
** Load the dataset policy information into a linked list.
**
** Dataset files are linked with two other linked lists within
**  the data: for the tables dataset_path_policy and
**  dataset_file_policy.
**
******************************************************************** */
static int cacheDatasetInfo(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    dataset_list_t **dataset)
{
    pnt_dataset_list_t ptr,last_ptr;
    pnt_ext_list_t pnt_ext;
    char qbuf[IMS_COL512_LEN+1];
    char *saveBuf;
    int status;
    int rowCount;
    int extCount;
    int i;
    CS_TINYINT  ttemp;
    pnt_dataset_list_t pnt_dataset;
    long  last_dataset_idx;
    long  num_datasets;


    /*
    ** First build the list of datasets
    */

    saveBuf = qDesc->cmd;
    qDesc->cmd = (char *) &qbuf[0];
    sprintf(qbuf, "select r.dataset_idx, p.granules_table, p.version_p,"
        " p.process_level, p.oagdr, p.mfd_p  from "
        "dataset_relation r, dataset_policy p where "
        "r.dataset_idx = p.dataset_idx");

    ptr = NULL;
    rowCount = 0;
    num_datasets = 0;
    last_dataset_idx = -1;
    last_ptr = NULL;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){

        if (status < IMS_OK){
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't cache dataset information. Query Failed.");
            qDesc->cmd = saveBuf;
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */

        if (status == IMS_ENDOFQUERY){
            continue;
        }

        rowCount++;
        num_datasets++;

        if (rowCount == 1){
            ptr = (void *) malloc(sizeof(dataset_list_t));

            if (ptr == NULL){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory to cache dataset info");

                qDesc->cmd = saveBuf;

                return(IMS_FATAL);
            }
            *dataset = ptr;
            last_ptr = ptr;
        }
        else  if(  ptr->pnt_next  !=  NULL ){
            last_ptr = ptr;
            ptr = ptr->pnt_next;
        }
        else{
            last_ptr = ptr;
            ptr->pnt_next = (void *) malloc(sizeof(dataset_list_t));

            if (ptr->pnt_next == NULL){
                (void) ims_msg(msgDesc, IMS_FATAL,
                    "Could not allocate memory to cache dataset info.");
                qDesc->cmd = saveBuf;
                return(IMS_FATAL);
            }
            ptr = ptr->pnt_next;
        }
        ptr->pnt_next = NULL;


        /*
        ** Get Dataset Index
        */
        memcpy((char *) &(ptr->dataset_idx), qDesc->valAddr[0],
            qDesc->valLength[0]);

        /*
        ** Get Granule Table Name
        */
        memset((char *) ptr->granules_table, 0, sizeof(
            ptr->granules_table));
        memcpy((char *) ptr->granules_table, qDesc->valAddr[1],
                    qDesc->valLength[1]);
        ims_trim(ptr->granules_table);

        /*
        ** Version Information
        */
        memcpy((char *) &(ptr->version_p), qDesc->valAddr[2],
            qDesc->valLength[2]);

        /*
        ** Process Level Information
        */
        memcpy((char *) &(ptr->process_level), qDesc->valAddr[3],
            qDesc->valLength[3]);

        /*
        ** oagdr: status flag
        */
        memcpy((char *) &(ttemp), qDesc->valAddr[4],
            qDesc->valLength[4]);
        ptr->oagdr = ttemp;

        /*
        ** mfd_p
        */
        memcpy((char *) ptr->mfd_p, qDesc->valAddr[5],
                    qDesc->valLength[5]);
        ptr->mfd_p[1] = '\0'; /* 1 character value  */

        /*
        **  we only want one entry per dataset, but dataset_relation
        **      has entries for each valid dataset name.
        */
        if(  ptr->dataset_idx  ==  last_dataset_idx  ){
            ptr = last_ptr;
            num_datasets--;
        }
        last_dataset_idx =  ptr->dataset_idx;
    }

    /*
    ** Reset Query Descriptor
    */
    if (ims_qiResetDesc(qDesc) < IMS_OK){
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        qDesc->cmd = saveBuf;
        return(IMS_FATAL);
    }

    /*
    ** Now, get the path information for each dataset.
    */
    pnt_dataset = *dataset;
    status = get_path_info( msgDesc, qDesc, num_datasets, pnt_dataset );
    if(  status  <  IMS_OK  ){
        qDesc->cmd = saveBuf;
        return( status );
    }

    /*
    ** Now, get the file policy information for each dataset.
    */
    status = get_file_info( msgDesc, qDesc, num_datasets, pnt_dataset );
    if(  status  <  IMS_OK  ){
        qDesc->cmd = saveBuf;
        return( status );
    }
    qDesc->cmd = saveBuf;
    return( IMS_OK );
}


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
    IMS_QI_DESC_OBJ *qDesc,
    long        num_datasets,
    pnt_dataset_list_t  pnt_datasets )
{
    int status;
    long  i, index;
    int pathCount;
    short  i_s;
    pnt_dataset_list_t  pnt_dataset;
    pnt_path_list_t ptrPath;


    pnt_dataset = pnt_datasets;

    for (i = 0; i < num_datasets ; i++) {
        sprintf(qDesc->cmd, "select path, start_granule_idx, "
            "end_granule_idx from dataset_path_policy where "
            "dataset_idx = %d", pnt_dataset->dataset_idx);

        pathCount = 0;
        pnt_dataset->pnt_path_list = NULL;

        while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
            if (status < IMS_OK){
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Can't cache dataset information. Query Failed.");
                return(IMS_ERROR);
            }

            /*
            ** Check for End of Query.
            */
            if (status == IMS_ENDOFQUERY)
            {
                continue;
            }

            pathCount++;
            if (pathCount == 1){
                ptrPath = (void *) malloc(sizeof(path_list_t));
                if (ptrPath == NULL){
                    (void) ims_msg(msgDesc, IMS_FATAL,
                        "Could not allocate memory to cache dataset "
                        "info");
                    return(IMS_FATAL);
                }
                pnt_dataset->pnt_path_list = ptrPath;
            }
            else{
                ptrPath->pnt_next = (void *) malloc(
                    sizeof(path_list_t));
                if (ptrPath->pnt_next == NULL){
                    (void) ims_msg(msgDesc, IMS_FATAL,
                        "Could not allocate memory to cache dataset "
                        "info.");
                    return(IMS_FATAL);
                }
                ptrPath = ptrPath->pnt_next;
            }
            ptrPath->pnt_next = NULL;
            ptrPath->path_done = IMS_FALSE;

            /*
            ** Get Dataset Path Specification
            */
            memset(ptrPath->path, 0, sizeof(ptrPath->path));

            memcpy(ptrPath->path, qDesc->valAddr[0],
                qDesc->valLength[0]);
            ims_trim(ptrPath->path);

            /*
            ** Get Start Granule Index
            */
            memcpy((char *) &(ptrPath->start_granule_idx),
                qDesc->valAddr[1], qDesc->valLength[1]);

            if (qDesc->valLength[1] == 0)
                ptrPath->start_granule_idx = -1;

            /*
            ** Get End Granule Index
            */
            memcpy((char *) &(ptrPath->end_granule_idx),
                qDesc->valAddr[2], qDesc->valLength[2]);

            if (qDesc->valLength[2] == 0)
                ptrPath->end_granule_idx = -1;
        }
        pnt_dataset = pnt_dataset->pnt_next;

        /*
        ** Reset Query Descriptor
        */
        if (ims_qiResetDesc(qDesc) < IMS_OK)
        {
            (void) ims_msg(msgDesc, IMS_FATAL,
                "Could not reset the query descriptor.");
            return(IMS_FATAL);
        }
    }
}   /*  get_path_info  */


/***************************************************************
**
**  subr get_file_info ()
**
**  This routine gets the dataset_file_policy information
**      for each dataset type.
**
**************************************************************** */
static  int get_file_info (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    long        num_datasets,
    pnt_dataset_list_t  pnt_datasets )
{
    int status;
    long  i, index;
    int extCount;
    short  i_s;
    pnt_dataset_list_t  pnt_dataset;
    pnt_ext_list_t pnt_ext;


    pnt_dataset = pnt_datasets;

    for (i = 0; i < num_datasets ; i++) {
        sprintf(qDesc->cmd, "select format, extension, type "
            "from dataset_file_policy where dataset_idx = %d",
            pnt_dataset->dataset_idx);

        extCount = 0;
        pnt_dataset->pnt_ext_list = NULL;

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
                pnt_dataset->pnt_ext_list = pnt_ext;
                pnt_ext->pnt_next = NULL;
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
        pnt_dataset = pnt_dataset->pnt_next;

        /*
        ** Reset Query Descriptor
        */

        if (ims_qiResetDesc(qDesc) < IMS_OK){
            (void) ims_msg(msgDesc, IMS_FATAL,
                "Could not reset the query descriptor.");
            return(IMS_FATAL);
        }
    }

    return(IMS_OK);
}   /*  get_file_info  */


/***************************************************************
**
** freeCacheData()
**
** This function will free the associated data structures for the cached
** dataset data.
**
**************************************************************** */

static int freeCacheData(
    IMS_MSG_STRUCT *msgDesc,
    pnt_dataset_list_t pnt_dataset)
{
    pnt_dataset_list_t ptr;
    pnt_ext_list_t pnt_ext;
    pnt_path_list_t ptrPath;

    ptr = pnt_dataset;
    while (ptr != NULL){
        pnt_ext = ptr->pnt_ext_list;

        while (pnt_ext != NULL){
            pnt_ext = pnt_ext->pnt_next;
            free(ptr->pnt_ext_list);
            ptr->pnt_ext_list = pnt_ext;
        }

        ptrPath = ptr->pnt_path_list;

        while (ptrPath != NULL)
        {
            ptrPath = ptrPath->pnt_next;
            free(ptr->pnt_path_list);
            ptr->pnt_path_list = ptrPath;
        }

        ptr = ptr->pnt_next;
        free(pnt_dataset);
        pnt_dataset = ptr;
    }
    return(IMS_OK);

}   /*  freeCacheData  */


/***************************************************************
**
** openConnection ()
**
** This function will open a connection to the SQL server.
**
**
**************************************************************** */

static int openConnection (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ **qDescPass,
    USER_SPEC *userSpec)
{
    IMS_QI_DESC_OBJ *qDesc;
    int status;


    /*
    ** Allocate a query descriptor
    */

    if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate a query descriptor.");
        return(IMS_ERROR);
    }

    qDesc->cmd = (char *) malloc(IMS_COL512_LEN);

    if ((char *) qDesc->cmd == NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate the command area for the "
            "query descriptor.");
         free(qDesc->cmd);
         (void) ims_qiFreeDesc(qDesc);
         return(IMS_ERROR);
    }

    IMS_SETUSER (qDesc, userSpec->username);
    IMS_SETPSWD (qDesc, userSpec->password);

    if (userSpec->program != NULL)
        IMS_SETPROG(qDesc, userSpec->program);

    if (userSpec->server != NULL)
        IMS_SETSERVER(qDesc, userSpec->server);

    if (userSpec->database != NULL)
        IMS_SETDBNAME(qDesc, userSpec->database);

    /*
    ** Attempt to logon to database
    */

    status = ims_qiLogin(qDesc);

    if (status < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Could not login to database server.");
        free(qDesc->cmd);
        (void) ims_qiFreeDesc(qDesc);
        return(IMS_ERROR);
    }

    IMS_SET_USERDATA(qDesc);

    *qDescPass = qDesc;  /* Set return query descriptor */
    return(IMS_OK);
}   /*  opernConnection   */


/****************************************************************
**
** closeConnection ()
**
** This function will close a connection to the SQL server.
**
**
***************************************************************** */

static int closeConnection (
    IMS_QI_DESC_OBJ *qDesc)
{

    free(qDesc->cmd);
    (void) ims_qiFreeDesc(qDesc);
    return(IMS_OK);
}   /*  closeConnection  */


/***************************************************************
**
**  subr go_through_datasets ()
**
**  This function will remove the products which are marked for delete
**  by updating the status in the granules table to DELETED and removing
**  the files from the repository.
**
**************************************************************** */

static int go_through_datasets(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    pnt_dataset_list_t pnt_datasets,
    long  startDataset,
    long  endDataset )
{
    char qbuf[IMS_COL512_LEN+1];
    char *saveBuf;
    char format[IMS_COL10_LEN+1];
    char name[IMS_COL30_LEN|1];
    int granule_idx;
    int status;
    int rowCount;
    short int version;
    pnt_dataset_list_t  pnt_dataset;
    pnt_dataset_list_t  pnt_dataset2;
    short  fts_status; /*  status of granule  */
    char  received_time[33];
    char  contributor[31];
    char  dir_name[256];
    pnt_path_list_t  pnt_path;
    pnt_ext_list_t  pnt_ext;
    pnt_ext_list_t  pnt_ext_1st;
    long  i,j,k,l;
    IMS_NUMERIC_DATE curr, received;
    int  diff; /* difference between current time and received_time */
    int  tdiff; /* temp storage: no. days difference  */
    char  full_path_name[256];
    long  n_item;
    char  curr_date[33];
    pnt_file_names_t  pnt_fnames, pnt_fnames_1st, pnt_fnames_temp;
    DIR  *pnt_directory;
    struct dirent *pnt_dirent;
    long  n_stragglers;
    long  n_stragglers_sum;
    short  flag, flag2;
    long  num_fnames;
    long  n_fnames_buf;
    long  n_dirs;

    saveBuf = qDesc->cmd;
    qDesc->cmd = (char *) &qbuf[0];
    pnt_dataset = pnt_datasets;
    pnt_fnames_1st = (pnt_file_names_t) malloc( sizeof( file_names_t ));
    pnt_fnames = pnt_fnames_1st;
    pnt_fnames->pnt_next = NULL;
    pnt_fnames->name[0][0] = '\0';
    n_stragglers_sum = 0;
    n_dirs = 0;

    while ( pnt_dataset != NULL){
        if(  pnt_dataset->dataset_idx  <  startDataset  ||
            pnt_dataset->dataset_idx  >  endDataset  ){
            pnt_dataset = pnt_dataset->pnt_next;
            continue;
        }

        /*
        **  now check all the directories for this granule.
        **      go through all the dataset_path_policy dir
        **      valid for this granule.
        */
        pnt_path = pnt_dataset->pnt_path_list;
        flag = IMS_TRUE;
        while( flag ){
            strcpy( dir_name, pnt_path->path );
            i = pnt_path->start_granule_idx;
            j = pnt_path->end_granule_idx;
            if(  pnt_path->path_done  ){
                /*
                **  in this implimentation, should never be here.
                */
                pnt_path = pnt_path->pnt_next;
                if(  pnt_path  ==  NULL  )  flag = IMS_FALSE;
                continue;
            }
            pnt_path->path_done = IMS_TRUE;
            /*
            **  now get the files for this directory.  put into
            **      a buffer.  when found, zero out the name.
            **      if not found, do not worry.
            */
            n_dirs++;
            pnt_fnames = pnt_fnames_1st;
            pnt_fnames->name[0][0] = '\0';
            num_fnames = 0;
            n_fnames_buf = 0;
            pnt_directory = opendir( dir_name );
            if(  pnt_directory  ==  NULL  ){
                (void)  ims_msg( msgDesc, IMS_ERROR,
                    "***  directory %s does not exist for "
                    "dataset_idx %d   ****", dir_name,
                    pnt_dataset->dataset_idx );
                pnt_path = pnt_path->pnt_next;
                if(  pnt_path  ==  NULL  )  flag = IMS_FALSE;
                continue;
            }
            pnt_dirent = NULL;
            flag2 = IMS_TRUE;
            while(  flag2  ){
                pnt_dirent = readdir( pnt_directory );
                if(  pnt_dirent  !=  NULL  ){
                    if(  pnt_dirent->d_name[0]  !=  '.'  ){
                        num_fnames++;
                        n_fnames_buf++;
                        if(  n_fnames_buf  ==  MAX_FNAMES  ){
                            /*
                            **  need new buffer: may already exist
                            */
                            if(  pnt_fnames->pnt_next  ==  NULL  ){
                                pnt_fnames_temp = (pnt_file_names_t)
                                    malloc( sizeof( file_names_t ) );
                                pnt_fnames_temp->pnt_next = NULL;
                                pnt_fnames->pnt_next = pnt_fnames_temp;
                                pnt_fnames = pnt_fnames_temp;
                            }
                            else  pnt_fnames = pnt_fnames->pnt_next;
                            pnt_fnames->name[0][0] = '\0';
                            n_fnames_buf = 1;
                        }
                        k = strlen( pnt_dirent->d_name );
                        if(  k  >  MAX_FNAME_LEN  ){
                            pnt_dirent->d_name[MAX_FNAME_LEN] = '\0';
                        }
                        strcpy( pnt_fnames->name[n_fnames_buf-1],
                            pnt_dirent->d_name );
                    }
                }
                else  flag2 = 0;
            }
            status = closedir( pnt_directory );

            /*
            **  now read all the granules that appply to this dir
            */
            if(  i  <  0  &&  j  <  0  ){
                sprintf(qbuf, "select name, granule_idx "
                    " from %s",
                    pnt_dataset->granules_table );
            }
            else  if(  i  <  0  ){
                sprintf(qbuf, "select name, granule_idx "
                    " from %s  where  granule_idx  <=  %ld",
                    pnt_dataset->granules_table, j );
            }
            else  if(  j  <  0  ){
                sprintf(qbuf, "select name, granule_idx "
                    " from %s  where  granule_idx  >=  %ld",
                    pnt_dataset->granules_table, i );
            }
            else{
                sprintf(qbuf, "select name, granule_idx "
                    " from %s  where  granule_idx  <=  %ld "
                    "  and  granule_idx  >=  %ld",
                    pnt_dataset->granules_table, j, i );
            }
            rowCount = 0;

            while ((status = ims_qiNextRow(qDesc)) !=
                IMS_ENDOFTRANSACTION){
                if (status < IMS_OK){
                    (void) ims_msg(msgDesc, IMS_ERROR,
                        "Can't determine granule information. "
                        "Query Failed.");
                    qDesc->cmd = saveBuf;
                    return(IMS_ERROR);
                }

                /*
                ** Check for End of Query.
                */
                if (status == IMS_ENDOFQUERY){
                    continue;
                }

                rowCount ++;

                /*
                ** Get product name
                */
                memset(name, 0, sizeof(name));
                memcpy(name, qDesc->valAddr[0], qDesc->valLength[0]);
                ims_trim(name);

                /*
                ** Get Granule Index
                */
                memcpy( &granule_idx, qDesc->valAddr[1],
                    qDesc->valLength[1]);

                /*
                **  get file name in list: if found, zero out name
                **      put dot at end for comparisons.
                */
                strcat( name, "." );
                n_fnames_buf = 0;
                pnt_fnames = pnt_fnames_1st;
                flag2 = IMS_FALSE;
                for(  j=0 ; j  <  num_fnames ; j++  ){
                    n_fnames_buf++;
                    if(  n_fnames_buf  ==  MAX_FNAMES ){
                        n_fnames_buf = 1;
                        pnt_fnames = pnt_fnames->pnt_next;
                    }
                    if( pnt_fnames->name[n_fnames_buf-1][0]  !=  '\0' ){
                        k = ims_strIndex(
                            pnt_fnames->name[n_fnames_buf-1], name );
                        if(  k  ==  0  ){
                            /*
                            **  probably found: need to check suffixes
                            */
                            pnt_ext = pnt_dataset->pnt_ext_list;
                            l = strlen( name );
                            while( pnt_ext  !=  NULL  ){
                                name[l] = '\0';
                                strcat( name, pnt_ext->extension );
                                if(  strcmp( name,
                                    pnt_fnames->name[n_fnames_buf-1] )
                                    ==  0  ){
                                    /*
                                    **  name found
                                    */
                                    pnt_fnames->name[n_fnames_buf-1][0]
                                        = '\0';
                                    flag2 = IMS_TRUE;
                                }
                                pnt_ext = pnt_ext->pnt_next;
                            }
                            name[l] = '\0';
                        }
                    }
                }
            }

            /*
            **  now go through all the file names, and the ones that are
            **      not blanked out are stragglers.
            */
            n_fnames_buf = 0;
            pnt_fnames = pnt_fnames_1st;
            flag2 = IMS_FALSE;
            n_stragglers = 0;
            for(  j=0 ; j  <  num_fnames ; j++  ){
                n_fnames_buf++;
                if(  n_fnames_buf  ==  MAX_FNAMES ){
                    n_fnames_buf = 1;
                    pnt_fnames = pnt_fnames->pnt_next;
                }
                if( pnt_fnames->name[n_fnames_buf-1][0]  !=  '\0'  ){
                    /*
                    **  have a straggler
                    */
                    n_stragglers++;
                    if(  n_stragglers  ==  1  ){
                        (void)  ims_msg( msgDesc, IMS_INFO,
                            "**********  stragglers for dataset %ld "
                            "follow from dir '%s'.  ********" ,
                            pnt_dataset->dataset_idx, dir_name );
                    }
                    (void) ims_msg( msgDesc, IMS_INFO,
                        "File '%s' has no granule row.",
                        pnt_fnames->name[n_fnames_buf-1] );
                }
            }
            if(  n_stragglers  ==  0  ){
                (void)  ims_msg( msgDesc, IMS_INFO,
                    "*****  no stragglers for dataset %ld "
                    "from dir '%s'." ,
                    pnt_dataset->dataset_idx, dir_name );
            }
            else{
                (void)  ims_msg( msgDesc, IMS_INFO,
                    "+++++++++++  %ld unknown files for dataset %ld.",
                    n_stragglers, pnt_dataset->dataset_idx );
                n_stragglers_sum += n_stragglers;
            }
        }


        /*
        ** Reset Query Descriptor
        */
        if (ims_qiResetDesc(qDesc) < IMS_OK){
            (void) ims_msg(msgDesc, IMS_FATAL,
                "Could not reset the query descriptor.");
            qDesc->cmd = saveBuf;
            return(IMS_FATAL);
        }

        pnt_dataset = pnt_dataset->pnt_next;
    }

    if(  n_stragglers_sum  ==  0  ){
        (void)  ims_msg( msgDesc, IMS_INFO,
            "**********  No stragglers found in %ld datasets."
            "  *********", n_dirs );
    }
    else{
        (void)  ims_msg( msgDesc, IMS_INFO,
            "**********  A total of %ld stragglers found in "
            "%ld datasets.  *********",
            n_stragglers_sum, n_dirs );
    }

    qDesc->cmd = saveBuf;
    return(IMS_OK);
}   /* go_through_datasets  */


/* *****************************************************************
**
**  subr  get_items_value is called to get all the names from
**      the items table given the name of type.
**
***************************************************************** */
int   get_items_value(
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char * type_name,
    char items_values[10][60] )
{
    int status;
    long  i,j;
    char  str[1024];
    short  i_s;


    (void) strcpy( str, "select instance, description from items  " );
    (void) strcat( str, " where   type  =  '" );
    (void) strcat( str, type_name );
    (void) strcat( str, "' " );
    (void) strcpy( qDesc->cmd, str );

    for( i=0 ; i  <  10 ; i++ )  items_values[i][0] = '\0';

    status = ims_qiResetDesc( qDesc );

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't determine granule information. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        /*
        ** Copy the returned data into the structure.
        */

        j = 0;                              /*  instance  */
        (void) memcpy ((char *) &i_s,
            qDesc->valAddr[j], qDesc->valLength[j]);

        j++;                                /*  description  */
        (void) memcpy ((char *) str,
            qDesc->valAddr[j], qDesc->valLength[j]);
        str[qDesc->valLength[j]] = '\0';
        (void) ims_truncStr( str );

        /*
        **  put the value in the correct index of the array: the index
        **  is the instance value
        */
        strcpy( items_values[i_s], str );

        /*
        ** Check the returned status value.
        */
        if( checkRetStatus (msgDesc, qDesc ) < IMS_OK){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_items_value had error." );
            return ( IMS_ERROR );
        }

    }
    return( IMS_OK ) ;
}   /*  get_items_value   */


/*******************************************************************
**
**  subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************** */

static int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc)
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
**************************************************************** */

static int execCmd (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc)
{
    int status;
    int severity;

    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            return (ims_msgGetSeverity (msgDesc));
        }
    }

    if (qDesc->msgNo != 0)
    {
        return (ims_msgGetSeverity (msgDesc));
    }


    /*
    ** Reset Query Descriptor
    */

    if (ims_qiResetDesc(qDesc) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    return (IMS_OK);
}
