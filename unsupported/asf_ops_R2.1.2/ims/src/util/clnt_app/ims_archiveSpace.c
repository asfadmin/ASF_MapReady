static char *sccs = "@(#)ims_archiveSpace.c	1.1  04/16/97";
/***********************************************************
**
** File:        ims_archiveSpace.c
**
** Function:    This application will search through the dataset and
**              determine the space availiable to the repository
**              directories being used.  Only one line per disk is
**              printed per type of file, with two examples of
**              usage of that disk.  the areas looked at are the
**              stagin_area table, the local_dir under user_profile
**              (where ftp files are stored), and the
**              dataset_path_policy areas.
**
**
** Author:      David Pass
**
** Date:        8/7/96
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

#define   u_long    unsigned long

#include <sys/types.h>
#include <sys/statvfs.h>

/*
** Local Definitions.
*/
#define DIR_NAME_LEN (size_t)30


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
typedef  struct  disk__t *pnt_disk_t;
typedef struct disk__t{
    char path1[IMS_COL255_LEN+1]; /* 1st and 2nd paths on disk */
    char path2[IMS_COL255_LEN+1];
    int  num_paths; /* no. paths which use this disk  */
    char host[31];  /* for stage_area  */
    short  dataset_idx; /* for dataset_path_policy  */
    unsigned long  total_blocks;/*  total no. blocks in system  */
    unsigned long  free_blocks; /* no. of free blocks  */
    unsigned long  free_nodes; /* no. of free nodes  */
    char disk_name[37];
    pnt_disk_t  pnt_next;
} disk_t;

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
    char *termOutput; /* if true, print to regular terminal output  */
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
    {"-T",              &commands.termOutput},
    {"+termOutput",     &commands.termOutput},
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
    {"termOutput",    &commands.termOutput},
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
static int closeConnection (IMS_QI_DESC_OBJ *);
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **,
    USER_SPEC *);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int checkRetStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int print_stage_area( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
     short );
static int print_dataset_area( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
     short );
static int print_user_ftp_area( IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
     short );

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
    long  i,j;
    short  print_output;
    FILE * out_file;


    commands.username = NULL;
    commands.password = NULL;
    commands.logFilePath = NULL;
    commands.logFileName = NULL;
    commands.termOutput = NULL;
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
        status = access( logFileSpec, F_OK );
        if(  status  !=  0 ){ /* file not there: create it  */
            if ((out_file = fopen (logFileSpec, "w")) ==  NULL){
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not open log file '%s'. %s",
                    logFileSpec, strerror (errno));
                goto ERROR;
            }
            (void) fclose( out_file );
        }
        if ((fd = open (logFileSpec, O_RDWR|O_CREAT|O_APPEND)) == -1){
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
        /*  (void) ims_msgStderrFlag (msgDesc, IMS_OFF); */

        /*
        ** Write a delimiting message to the log file.
        */
        (void) sprintf (writeBuffer,
            "\n\n>>>>>>>>>>>>>>>>>>  IMS DISK CHECK STARTUP  "
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
    **  turn off the standard error output
    */
    status = ims_msgStderrFlag( msgDesc, IMS_OFF );

    if (openConnection(msgDesc, &qDesc, &userSpec) < IMS_OK){
        goto ERROR;
    }

    /*
    ** Okay, go do the work...
    */
    print_output = IMS_TRUE;
    if(  commands.termOutput  !=  NULL  ){
        /*
        **  N or n for terminal to not get output
        */
        if(  commands.termOutput[0]  ==  'N'  ||
            commands.termOutput[0]  ==  'n'  ) print_output = IMS_FALSE;
    }
    status = print_stage_area( msgDesc, qDesc, print_output );
    if(  status  <  IMS_OK ){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Could not get data for stage areas." );
        goto  ERROR;
    }

    status = print_dataset_area( msgDesc, qDesc, print_output );
    if(  status  <  IMS_OK ){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Could not get data for dataset areas." );
        goto  ERROR;
    }

    status = print_user_ftp_area( msgDesc, qDesc, print_output );
    if(  status  <  IMS_OK ){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Could not get data for user's FTP areas." );
        goto  ERROR;
    }
    closeConnection(qDesc);

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "The ims_archiveSpace processing is complete.");

    (void) ims_msgStructFree (msgDesc);
    (void) close (fd);

    exit (0);

ERROR:
    /*
    ** Shutdown the message facility.
    */
    status = ims_msgStderrFlag( msgDesc, IMS_ON );
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The ims_archiveSpace processing failed.");

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
    else{
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
    else{
        logInfo->logFlag = IMS_FALSE;
    }

    /* termOutput  */
    if (commands.termOutput  ==  (char *) NULL)
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Terminal output (Y or N): ") == (char *) NULL)
                {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }
        i = strlen( inputBuffer )+1;
        commands.termOutput = (char *) malloc( 2 * sizeof( char ));
        commands.termOutput[0] = inputBuffer[0];
        commands.termOutput[1] = '\0';
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


/***************************************************************
**
**  subr print_stage_area ()
**
**  This routine gets the stage_area information
**
**************************************************************** */
static  int print_stage_area (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    short  print_output )
{
    int status;
    long  i, index;
    int pathCount;
    short  i_s;
    pnt_disk_t  pnt_disk_1st, pnt_disk, pnt_disk2;
    short  num_disks;
    char  str[256], str2[256], str3[256];
    short  flag;
    struct  statvfs  statvfs_buf;


    pnt_disk_1st = NULL;

    sprintf(qDesc->cmd, "select path, host  from stage_area" );

    num_disks = 0;
    pathCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't select stage_area. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        pathCount++;

        /*
        ** Get path
        */
        memcpy( str, qDesc->valAddr[0],
            qDesc->valLength[0]);
        str[ qDesc->valLength[0] ] = '\0';
        ims_trim( str );

        /*
        ** Get host
        */
        memcpy( str2, qDesc->valAddr[1],
            qDesc->valLength[1]);
        str2[ qDesc->valLength[1] ] = '\0';
        ims_trim( str2 );

        /*
        **  path should start with a /: print error is not
        */
        if(  str[0]  !=  '/' ){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Path from stage_area does not start with a slash: %s",
                str );
            return ( IMS_ERROR );
        }
        i = ims_strIndex( str+1, "/" );
        if(  i  ==  -1  )  i = strlen( str );
        strcpy( str3, str );
        str3[i+1] = '\0';

        if (pathCount == 1)   flag = IMS_TRUE;
        else{
            /*
            **  check if this path is already defined
            */
            pnt_disk2 = pnt_disk_1st;
            flag = IMS_TRUE;
            while( pnt_disk2  !=  NULL ){
                if(  strcmp( str3, pnt_disk2->disk_name )  ==  0  ){
                    /*
                    **  have a match: check if two yet
                    */
                    if(  pnt_disk2->path2[0]  ==  '\0' ){
                        /*
                        **  make sure different from path1.
                        */
                        if(  strcmp( pnt_disk2->path1, str )
                            !=  0  )
                            strcpy( pnt_disk2->path2, str );
                    }
                    pnt_disk2->num_paths++;
                    flag = IMS_FALSE;
                }
                pnt_disk2 = pnt_disk2->pnt_next;
            }
        }
        if(  flag ){
            /*
            **  this is a new disk: put it in.
            */
            num_disks++;
            pnt_disk2 = (pnt_disk_t) malloc( sizeof( disk_t ) );
            pnt_disk2->path1[0] = '\0';
            pnt_disk2->path2[0] = '\0';
            pnt_disk2->num_paths = 1;
            pnt_disk2->host[0] = '\0';
            pnt_disk2->dataset_idx = -1;
            pnt_disk2->total_blocks = 0;
            pnt_disk2->free_blocks = 0;
            pnt_disk2->free_nodes = 0;
            pnt_disk2->disk_name[0] = '\0';
            pnt_disk2->pnt_next = NULL;
            if(  pathCount  ==  1  )  pnt_disk_1st = pnt_disk2;
            else  pnt_disk->pnt_next = pnt_disk2;
            pnt_disk = pnt_disk2;
            strcpy( pnt_disk->host, str2 );
            strcpy( pnt_disk->path1, str );
            strcpy( pnt_disk->disk_name, str3 );
        }
    }

    /*
    ** Reset Query Descriptor
    */
    if (ims_qiResetDesc(qDesc) < IMS_OK){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    /*
    **  now print the results:  go through the disks and print
    **      them.  different to the terminal if asked for.
    */
    pnt_disk = pnt_disk_1st;
    if(  print_output ){
        printf( "\n          ********  Data from stage_area\n\n" );
        printf( "   DISK            TOTAL BLKS   FREE BLKS  %%    "
            "FREE NODES   FILE NAMES (2 max)\n" );
    }
    while( pnt_disk  !=  NULL  ){
        status = statvfs( pnt_disk->path1, &statvfs_buf );
        if(  status  !=  0  ){
            (void)  ims_msg( msgDesc, IMS_WARNING,
                "****  Disk %s not mounted on this machine. ****",
                pnt_disk->disk_name );
            if(  print_output ){
                printf( "****  Disk %s not mounted on this machine."
                    " ****\n", pnt_disk->disk_name );
            }
            pnt_disk = pnt_disk->pnt_next;
            continue;
        }
        pnt_disk->total_blocks = statvfs_buf.f_blocks;
        pnt_disk->free_blocks = statvfs_buf.f_bfree;
        pnt_disk->free_nodes = statvfs_buf.f_ffree;
        if(  statvfs_buf.f_ffree  >  2000000000  )
            pnt_disk->free_nodes = 2000000000;
        i = pnt_disk->free_blocks*100/pnt_disk->total_blocks;

        sprintf( str, "Disk %-15s has %10ld total blocks, "
            "%10ld free blocks (%3ld percent), total free nodes %ld.",
            pnt_disk->disk_name, pnt_disk->total_blocks,
            pnt_disk->free_blocks, i, pnt_disk->free_nodes );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str );
        sprintf( str2, "     No. paths = %d    path names (2 max): "
            "%s     %s", pnt_disk->num_paths, pnt_disk->path1,
            pnt_disk->path2 );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str2 );

        if(  print_output ){
            printf( "%-15s  %10ld   %10ld  %3ld   %10ld   %s    %s\n",
                pnt_disk->disk_name, pnt_disk->total_blocks,
                pnt_disk->free_blocks, i, pnt_disk->free_nodes,
                pnt_disk->path1, pnt_disk->path2 );

        }
        pnt_disk = pnt_disk->pnt_next;
    }
    return( IMS_OK );
}   /*  print_stage_area  */


/***************************************************************
**
**  subr print_dataset_area ()
**
**  This routine gets the dataset_path_policy information
**      for each dataset type.
**
**************************************************************** */
static  int print_dataset_area (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    short  print_output )
{
    int status;
    long  i, index;
    int pathCount;
    short  i_s;
    pnt_disk_t  pnt_disk_1st, pnt_disk, pnt_disk2;
    short  num_disks;
    char  str[256], str2[256], str3[256];
    short  flag;
    struct  statvfs  statvfs_buf;


    pnt_disk_1st = NULL;

    sprintf(qDesc->cmd, "select path, dataset_idx  from "
        "dataset_path_policy" );

    num_disks = 0;
    pathCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't select dataset area. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        pathCount++;

        /*
        ** Get path
        */
        memcpy( str, qDesc->valAddr[0],
            qDesc->valLength[0]);
        str[ qDesc->valLength[0] ] = '\0';
        ims_trim( str );

        /*
        ** Get dataset_idx
        */
        memcpy( &i_s, qDesc->valAddr[1],
            qDesc->valLength[1]);

        /*
        **  path should start with a /: print error is not
        */
        if(  str[0]  !=  '/' ){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Path from dataset area does not start with a slash: %s"
                , str );
            return ( IMS_ERROR );
        }
        i = ims_strIndex( str+1, "/" );
        if(  i  ==  -1  )  i = strlen( str );
        strcpy( str3, str );
        str3[i+1] = '\0';

        if (pathCount == 1)   flag = IMS_TRUE;
        else{
            /*
            **  check if this path is already defined
            */
            pnt_disk2 = pnt_disk_1st;
            flag = IMS_TRUE;
            while( pnt_disk2  !=  NULL ){
                if(  strcmp( str3, pnt_disk2->disk_name )  ==  0  ){
                    /*
                    **  have a match: check if two yet
                    */
                    if(  pnt_disk2->path2[0]  ==  '\0' ){
                        /*
                        **  make sure different from path1.
                        */
                        if(  strcmp( pnt_disk2->path1, str )
                            !=  0  )
                            strcpy( pnt_disk2->path2, str );
                    }
                    pnt_disk2->num_paths++;
                    flag = IMS_FALSE;
                }
                pnt_disk2 = pnt_disk2->pnt_next;
            }
        }
        if(  flag ){
            /*
            **  this is a new disk: put it in.
            */
            num_disks++;
            pnt_disk2 = (pnt_disk_t) malloc( sizeof( disk_t ) );
            pnt_disk2->path1[0] = '\0';
            pnt_disk2->path2[0] = '\0';
            pnt_disk2->num_paths = 1;
            pnt_disk2->host[0] = '\0';
            pnt_disk2->dataset_idx = -1;
            pnt_disk2->total_blocks = 0;
            pnt_disk2->free_blocks = 0;
            pnt_disk2->disk_name[0] = '\0';
            pnt_disk2->pnt_next = NULL;
            if(  pathCount  ==  1  )  pnt_disk_1st = pnt_disk2;
            else  pnt_disk->pnt_next = pnt_disk2;
            pnt_disk = pnt_disk2;
            pnt_disk->dataset_idx = i_s;
            strcpy( pnt_disk->path1, str );
            strcpy( pnt_disk->disk_name, str3 );
        }
    }

    /*
    ** Reset Query Descriptor
    */
    if (ims_qiResetDesc(qDesc) < IMS_OK){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    /*
    **  now print the results:  go through the disks and print
    **      them.  different to the terminal if asked for.
    */
    pnt_disk = pnt_disk_1st;
    if(  print_output ){
        printf("\n\n          ********  Data from dataset_path_policy table\n\n");
        printf( "   DISK            TOTAL BLKS   FREE BLKS  %%    "
            "FREE NODES   FILE NAMES (2 max)\n" );
    }
    while( pnt_disk  !=  NULL  ){
        status = statvfs( pnt_disk->path1, &statvfs_buf );
        if(  status  !=  0  ){
            (void)  ims_msg( msgDesc, IMS_WARNING,
                "****  Disk %s not mounted on this machine. ****",
                pnt_disk->disk_name );
            if(  print_output ){
                printf( "****  Disk %s not mounted on this machine."
                    " ****\n", pnt_disk->disk_name );
            }
            pnt_disk = pnt_disk->pnt_next;
            continue;
        }
        pnt_disk->total_blocks = statvfs_buf.f_blocks;
        pnt_disk->free_blocks = statvfs_buf.f_bfree;
        pnt_disk->free_nodes = statvfs_buf.f_ffree;
        if(  statvfs_buf.f_ffree  >  2000000000  )
            pnt_disk->free_nodes = 2000000000;
        i = pnt_disk->free_blocks*100/pnt_disk->total_blocks;

        sprintf( str, "Disk %-15s has %10ld total blocks, "
            "%10ld free blocks (%3ld percent), total free nodes %ld.",
            pnt_disk->disk_name, pnt_disk->total_blocks,
            pnt_disk->free_blocks, i, pnt_disk->free_nodes );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str );

        sprintf( str2, "     No. paths = %d    path names (2 max): "
            "%s     %s", pnt_disk->num_paths, pnt_disk->path1,
            pnt_disk->path2 );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str2 );

        if(  print_output ){
            printf( "%-15s  %10ld   %10ld  %3ld   %10ld   %s    %s\n",
                pnt_disk->disk_name, pnt_disk->total_blocks,
                pnt_disk->free_blocks, i, pnt_disk->free_nodes,
                pnt_disk->path1, pnt_disk->path2 );
        }
        pnt_disk = pnt_disk->pnt_next;
    }
    return( IMS_OK );
}   /*  print_dataset_area  */


/***************************************************************
**
**  subr print_user_ftp_area ()
**
**  This routine gets the local_dir from the user_profile table
**
**************************************************************** */
static  int print_user_ftp_area (
    IMS_MSG_STRUCT  *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    short  print_output )
{
    int status;
    long  i, index;
    int pathCount;
    short  i_s;
    pnt_disk_t  pnt_disk_1st, pnt_disk, pnt_disk2;
    short  num_disks;
    char  str[256], str2[256], str3[256];
    short  flag;
    struct  statvfs  statvfs_buf;


    pnt_disk_1st = NULL;

    sprintf(qDesc->cmd, "select ftp_dir, user_id  from user_profile");

    num_disks = 0;
    pathCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION){
        if (status < IMS_OK){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Can't select user_profile. Query Failed.");
            return(IMS_ERROR);
        }

        /*
        ** Check for End of Query.
        */
        if (status == IMS_ENDOFQUERY){
            continue;
        }

        pathCount++;

        /*
        ** Get ftp_dir (put in path).  if not set, skip.
        */
        memcpy( str, qDesc->valAddr[0],
            qDesc->valLength[0]);
        str[ qDesc->valLength[0] ] = '\0';
        ims_trim( str );
        if(  strlen( str )  ==  0  ){
            pathCount--;
            continue;
        }

        /*
        ** Get user_id
        */
        memcpy( str2, qDesc->valAddr[1],
            qDesc->valLength[1]);
        str2[ qDesc->valLength[1] ] = '\0';
        ims_trim( str2 );

        /*
        **  path should start with a /: print error is not
        */
        if(  str[0]  !=  '/' ){
            status = ims_msgStderrFlag( msgDesc, IMS_ON );
            (void) ims_msg( msgDesc, IMS_ERROR,
                "Path from user_profile area does not start with a "
                "slash: %s", str );
            return ( IMS_ERROR );
        }
        i = ims_strIndex( str+1, "/" );
        if(  i  ==  -1  )  i = strlen( str );
        strcpy( str3, str );
        str3[i+1] = '\0';

        if (pathCount == 1)   flag = IMS_TRUE;
        else{
            /*
            **  check if this path is already defined
            */
            pnt_disk2 = pnt_disk_1st;
            flag = IMS_TRUE;
            while( pnt_disk2  !=  NULL ){
                if(  strcmp( str3, pnt_disk2->disk_name )  ==  0  ){
                    /*
                    **  have a match: check if two yet
                    */
                    if(  pnt_disk2->path2[0]  ==  '\0' ){
                        /*
                        **  make sure different from host.
                        */
                        if(  strcmp( pnt_disk2->host, str2 )
                            !=  0  )
                            strcpy( pnt_disk2->path2, str2 );
                    }
                    pnt_disk2->num_paths++;
                    flag = IMS_FALSE;
                }
                pnt_disk2 = pnt_disk2->pnt_next;
            }
        }
        if(  flag ){
            /*
            **  this is a new disk: put it in.
            */
            num_disks++;
            pnt_disk2 = (pnt_disk_t) malloc( sizeof( disk_t ) );
            pnt_disk2->path1[0] = '\0';
            pnt_disk2->path2[0] = '\0';
            pnt_disk2->num_paths = 1;
            pnt_disk2->host[0] = '\0';
            pnt_disk2->dataset_idx = -1;
            pnt_disk2->total_blocks = 0;
            pnt_disk2->free_blocks = 0;
            pnt_disk2->disk_name[0] = '\0';
            pnt_disk2->pnt_next = NULL;
            if(  pathCount  ==  1  )  pnt_disk_1st = pnt_disk2;
            else  pnt_disk->pnt_next = pnt_disk2;
            pnt_disk = pnt_disk2;
            strcpy( pnt_disk->host, str2 );
            strcpy( pnt_disk->path1, str );
            strcpy( pnt_disk->disk_name, str3 );
        }
    }

    /*
    ** Reset Query Descriptor
    */
    if (ims_qiResetDesc(qDesc) < IMS_OK){
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    /*
    **  now print the results:  go through the disks and print
    **      them.  different to the terminal if asked for.
    */
    pnt_disk = pnt_disk_1st;
    if(  print_output ){
        printf( "\n\n          ********  Data from user_profile table\n\n" );
        printf( "   DISK            TOTAL BLKS   FREE BLKS  %%    "
            "FREE NODES   user_id names (2 max)\n" );
    }
    while( pnt_disk  !=  NULL  ){
        status = statvfs( pnt_disk->path1, &statvfs_buf );
        if(  status  !=  0  ){
            (void)  ims_msg( msgDesc, IMS_WARNING,
                "****  Disk %s not mounted on this machine. ****",
                pnt_disk->disk_name );
            if(  print_output ){
                printf( "****  Disk %s not mounted on this machine."
                    " ****\n", pnt_disk->disk_name );
            }
            pnt_disk = pnt_disk->pnt_next;
            continue;
        }
        pnt_disk->total_blocks = statvfs_buf.f_blocks;
        pnt_disk->free_blocks = statvfs_buf.f_bfree;
        pnt_disk->free_nodes = statvfs_buf.f_ffree;
        if(  statvfs_buf.f_ffree  >  2000000000  )
            pnt_disk->free_nodes = 2000000000;
        i = pnt_disk->free_blocks*100/pnt_disk->total_blocks;

        sprintf( str, "Disk %-15s has %10ld total blocks, "
            "%10ld free blocks (%3ld percent), total free nodes %ld.",
            pnt_disk->disk_name, pnt_disk->total_blocks,
            pnt_disk->free_blocks, i, pnt_disk->free_nodes );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str );

        sprintf( str2, "     No. paths = %d    user_id(2 max): %s   %s",
            pnt_disk->num_paths, pnt_disk->host, pnt_disk->path2 );
        (void)  ims_msg( msgDesc, IMS_INFO, "%s", str2 );

        if(  print_output ){
            printf( "%-15s  %10ld   %10ld  %3ld   %10ld   %s    %s\n",
                pnt_disk->disk_name, pnt_disk->total_blocks,
                pnt_disk->free_blocks, i, pnt_disk->free_nodes,
                pnt_disk->host, pnt_disk->path2 );
        }
        pnt_disk = pnt_disk->pnt_next;
    }
    if(  print_output ){
        printf( "\n\n   Note:  blocks are 1024 bytes.\n\n" );
    }
    return( IMS_OK );
}   /*  print_user_ftp_area  */


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
        status = ims_msgStderrFlag( msgDesc, IMS_ON );
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not reset the query descriptor.");
        return(IMS_FATAL);
    }

    return (IMS_OK);
}
