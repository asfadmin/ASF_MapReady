static char *sccs = "@(#)ims_mediaClnt.c	5.3  03/07/97";
/*******************************************************************
**
** File:        ims_mediaClnt.c
**
** Function:    Media distribution client program.  This program calls
**              the ims_mediaDist() function to distribute a list of
**              items onto the requested media.
**
** Author:      Sean Hardman
**
** Date:        5/19/95
**
** Modified:    1/23/97 - D. Pass - R2.1
**                    Added option to discontinue tape size check.
**                      The reportType is >= 100 in this case.
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
#include <ims_media.h>
#include <ims_tar.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static int checkTypes (IMS_MSG_STRUCT *);
static void freeDeviceInfo (DEVICE_INFO *);
static void freeItemList (MEDIA_ITEM_LIST *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
    char *username;
    char *password;
    char *mediaType;
    char *mediaFormat;
    char *reportType;
    char *noSpaceCheck;
    char *orderId;
    char *itemCount;
    char *itemList;
    char *itemFile;
    char *targetPath;
    char *archiveCheck;
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
    {"-M",           &commands.mediaType},
    {"+mediaType",   &commands.mediaType},
    {"-F",           &commands.mediaFormat},
    {"+mediaFormat", &commands.mediaFormat},
    {"-R",           &commands.reportType},
    {"+reportType",  &commands.reportType},
    {"-O",           &commands.orderId},
    {"+orderId",     &commands.orderId},
    {"-I",           &commands.itemCount},
    {"+itemCount",   &commands.itemCount},
    {"-L",           &commands.itemList},
    {"+itemList",    &commands.itemList},
    {"-D",           &commands.itemFile},
    {"+itemFile",    &commands.itemFile},
    {"-T",           &commands.targetPath},
    {"+targetPath",  &commands.targetPath},
    {"-A",           &commands.archiveCheck},
    {"+archiveCheck",&commands.archiveCheck},
    {"-S",           &commands.noSpaceCheck},
    {"+spaceCheck",  &commands.noSpaceCheck},
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

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
    {"username",    &commands.username},
    {"password",    &commands.password},
    {"mediaType",   &commands.mediaType},
    {"mediaFormat", &commands.mediaFormat},
    {"reportType",  &commands.reportType},
    {"orderId",     &commands.orderId},
    {"itemCount",   &commands.itemCount},
    {"itemList",    &commands.itemList},
    {"itemFile",    &commands.itemFile},
    {"targetPath",  &commands.targetPath},
    {"archiveCheck",&commands.archiveCheck},
    {"noSpaceCheck",&commands.noSpaceCheck},
    {"server",      &commands.server},
    {"database",    &commands.database},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *mediaTypeStr;
static char *mediaFormatStr;
static char *reportTypeStr;
static DBSMALLINT mediaType;
static DBSMALLINT mediaFormat;
static DBINT orderId;
static DBSMALLINT itemCount = 0;
static MEDIA_ITEM_LIST *itemList = (MEDIA_ITEM_LIST *) NULL;
static DBSMALLINT reportType;
static char *targetPath = (char *) NULL;
static char *archiveCheck = (char *) NULL;
static MEDIA_USER_SPEC userSpec;

/*******************************************************************
**
** main ()
**
******************************************************************** */

void main (
    int argc,
    char *argv[])
{
    IMS_MSG_STRUCT *msgDesc;
    DEVICE_INFO *deviceInfo;
    pnt_vdf_cat_request_t pnt_vdfReq;
    int status;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    char qcSpec[IMS_PATH_LEN+1];
    char stagePath[IMS_PATH_LEN+1];
    char mediaId[IMS_COL15_LEN+1];
    int tryCount;
    int mountStatus;
    int connectFlag;
    int qcFlag;
    int ceosFlag;


    /*
    ** Initialize variables.
    */
    deviceInfo = (DEVICE_INFO *) NULL;
    connectFlag = IMS_FALSE;
    ceosFlag = IMS_TRUE;

    commands.username = NULL;
    commands.password = NULL;
    commands.mediaType = NULL;
    commands.mediaFormat = NULL;
    commands.reportType = NULL;
    commands.orderId = NULL;
    commands.itemCount = NULL;
    commands.itemList = NULL;
    commands.itemFile = NULL;
    commands.targetPath = NULL;
    commands.archiveCheck = NULL;
    commands.noSpaceCheck = NULL;
    commands.server = NULL;
    commands.database = NULL;

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
    if (ims_setWrapup (runDown) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Initialization of the signal handler failed. %s.",
            strerror (errno));
        (void) ims_msgStructFree (msgDesc);
        exit (1);
    }

    /*
    ** Get the command line arguments. The variable status will actually
    ** contain the number of command line arguments processed upon
    ** successful completion.
    */
    if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
        cmdLineElmCount, msgDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "An error occurred parsing the command-line.");
        (void) ims_msgStructFree (msgDesc);
        exit (1);
    }

    /*
    ** Check to see if we got everything off of the command line.
    */
    if (status < argc)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Only %d out of the %d command line arguments were "
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
    ** If there is a command file present, then get any commands from
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
            (void) ims_msgStructFree (msgDesc);
            exit (1);
        }

        /*
        ** Now, get command line args again to overlay file arguments.
        */
        if ((status = ims_getCmdLine (argc, argv,
            cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "An error occurred parsing the command-line.");
            (void) ims_msgStructFree (msgDesc);
            exit (1);
        }
    }

    /*
    ** Process the information from the command-line and/or file.
    */
    if ((status = getArgInput (msgDesc)) < IMS_OK)
    {
        (void) ims_msgStructFree (msgDesc);
        exit (1);
    }

    /*
    ** Allocate space for the DEVICE_INFO structure.
    **
    ** lint: pointer cast may result in improper alignment
    ** No problem, malloc() aligns on worst case boundary.
    */
    if ((deviceInfo = (DEVICE_INFO *) malloc
        ((size_t) sizeof (DEVICE_INFO))) ==
        (DEVICE_INFO *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for the DEVICE_INFO structure.");
        (void) ims_msgStructFree (msgDesc);
        exit (1);
    }

    (void) memset (deviceInfo, 0, (size_t) sizeof (DEVICE_INFO));

    /*
    ** If we are distributing to tape then do the following.
    */
    if (mediaType != IMS_DISK)
    {
        /*
        ** If a targetPath was not provided then we
        ** need to allocate a device from the catalog.
        */
        if (targetPath == (char *) NULL)
        {
            /*
            ** Allocate a tape device based on mediaType requested.
            */
            if ((status = ims_deviceAlloc (msgDesc, (char *) &userSpec,
                mediaType, orderId, deviceInfo)) < IMS_OK)
            {
                freeDeviceInfo (deviceInfo);
                (void) ims_msgStructFree (msgDesc);
                exit (1);
            }

            /*
            ** Prompt for tape to be mounted.
            */
            tryCount = 0;
            do
            {
                (void) fprintf (stdout,
                    "\nPlease mount tape in '%s', then type <Return>",
                    deviceInfo->name);
                (void) gets (inputBuffer);

                mountStatus = ims_deviceTapeCheck (msgDesc, deviceInfo);
                tryCount += 1;
            } while ((mountStatus == IMS_WARNING) &&
                (tryCount != IMS_MAXTRIES));

            if (mountStatus < IMS_OK)
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not mount tape in '%s'.",
                    deviceInfo->name);
                (void) ims_deviceFree (msgDesc, (char *) &userSpec,
                    deviceInfo->device_id);
                freeDeviceInfo (deviceInfo);
                (void) ims_msgStructFree (msgDesc);
                exit (1);
            }
        }
        else  /* A tape device path was given. */
        {
            /*
            ** Populate the deviceInfo stucture with
            ** the provided path and a dummy device id.
            */
            deviceInfo->device_id = (DBSMALLINT) -1;
            (void) strcpy (deviceInfo->name, "Device -1");
            (void) strcpy (deviceInfo->path, targetPath);
        }
    }
    else /* The media type is DISK. */
    {
        /*
        ** Allocate an FTP device for the DISK device.
        ** This just returns the information for the device
        ** without updating the status for the device.
        ** This will not need to be freed.
        */
        if ((status = ims_deviceAlloc (msgDesc, (char *) &userSpec,
            mediaType, orderId, deviceInfo)) < IMS_OK)
        {
            freeDeviceInfo (deviceInfo);
            (void) ims_msgStructFree (msgDesc);
            exit (1);
        }
    }

    /*
    ** Allocate space for the vdf_cat_request_t structure.
    */
    if ((pnt_vdfReq = (pnt_vdf_cat_request_t) malloc
        ((size_t) sizeof (vdf_cat_request_t))) ==
        (pnt_vdf_cat_request_t) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for vdf_cat_request_t "
            "structure.");
        status = IMS_FATAL;
        goto ERROR;
    }

    (void) memset (pnt_vdfReq, 0, (size_t) sizeof (vdf_cat_request_t));

    /*
    ** Assign request structure values.
    */
    pnt_vdfReq->msgDesc = msgDesc;
    (void) strcpy (pnt_vdfReq->username, userSpec.username);
    (void) strcpy (pnt_vdfReq->password, userSpec.password);
    (void) strcpy (pnt_vdfReq->programName, userSpec.program);
    if (userSpec.server != (char *) NULL)
    {
        (void) strcpy (pnt_vdfReq->catSrvName, userSpec.server);
    }
    if (userSpec.database != (char *) NULL)
    {
        (void) strcpy (pnt_vdfReq->catDbName, userSpec.database);
    }

    /*
    ** Open a connection to the database server.
    */
    if((status = ims_vdfCat (pnt_vdfReq, VDF_OPEN_CONNECTION)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not open a database server connection.");
        goto ERROR;
    }
    else
    {
        connectFlag = IMS_TRUE;
    }

    /*
    ** Get the next media identifier.
    */
    if ((status = ims_deviceMediaId (msgDesc, (char *) &userSpec,
        mediaType, mediaFormat, mediaId, IMS_FALSE)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain the media identifier for the target "
            "media.");
        goto ERROR;
    }

    /*
    ** Record the beginning of the media job.
    ** Set-up the necessary variables first.
    */
    pnt_vdfReq->order_id = orderId;
    pnt_vdfReq->media_type = mediaType;
    pnt_vdfReq->device_id = deviceInfo->device_id;
    (void) strcpy (pnt_vdfReq->media_id, mediaId);

    if ((status = ims_vdfCat (pnt_vdfReq, VDF_START_JOB)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not record the beginning of the media job in the "
            "catalog.");
        goto ERROR;
    }

    /*
    ** Generate and distribute products onto media.
    **
    **  Put in flag for no tape space check if input
    */
    if(  commands.noSpaceCheck  !=  NULL  ){
        if(  commands.noSpaceCheck[0]  ==  'y'  ||
          commands.noSpaceCheck[0]  ==  'Y'  )  reportType += 100;
    }

    if ((status = ims_mediaDist (msgDesc, pnt_vdfReq,
        mediaType, mediaFormat, orderId, itemList, deviceInfo,
        qcSpec, reportType, &ceosFlag)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Media distribution failed.");
        pnt_vdfReq->status = MEDIA_JOB_FAILED;
        (void) ims_vdfCat (pnt_vdfReq, VDF_END_JOB);
        goto ERROR;
    }
    if(  reportType  >=  100  )  reportType -= 100;

    /*
    ** Perform a quality check if applicable.
    */
    if (reportType != NO_REPORT_TYPE)
    {
        /*
        ** Check for CEOS products.
        */
        if (ceosFlag == IMS_FALSE)
        {
            (void) ims_msg (msgDesc, IMS_WARNING,
                "The quality check report cannot be performed on "
                "non-CEOS products. This distribution contains non-CEOS"
                " products.");
        }
        else /* All products are in CEOS format. */
        {
            /*
            ** The archiveCheck argument determines whether the quality
            ** check is performed on the files in the local archive or
            ** whether it is performed on the target media.
            */
            if (archiveCheck == (char *) NULL)
            {
                qcFlag = IMS_FALSE;
            }
            else
            {
                if((archiveCheck[0] == 'y') || (archiveCheck[0] == 'Y'))
                {
                    qcFlag = IMS_TRUE;
                }
                else
                {
                    qcFlag = IMS_FALSE;
                }
            }

            /*
            ** If we generated a CEOS tape then the quality check
            ** should be performed on the target tape.
            */
            if ((mediaFormat == IMS_CEOS) && (mediaType != IMS_DISK) &&
                (qcFlag == IMS_FALSE))
            {
                (void) strcpy (qcSpec, deviceInfo->path);
            }

            /*
            ** Perform the quality check.
            */
            if ((status = ims_qc (msgDesc, (char *) &userSpec,
                reportType, qcSpec, mediaId, qcFlag)) < IMS_OK)
            {
                (void) ims_msg (msgDesc, status,
                    "Quality check failed.");
                pnt_vdfReq->status = MEDIA_JOB_FAILED;
                (void) ims_vdfCat (pnt_vdfReq, VDF_END_JOB);

                /* Remove temporary un-TAR area. */
                if ((mediaFormat == IMS_TAR) && (mediaType != IMS_DISK))
                {
                    (void) ims_extractPath (qcSpec, stagePath);
                    if ((status = ims_removeStageDir (msgDesc,
                        stagePath)) < IMS_OK)
                    {
                        (void) ims_msg (msgDesc, status,
                             "Could not remove the Quality Check "
                             "stage path '%s'", stagePath);
                    }
                }
                goto ERROR;
            }

            /*
            ** Remove temporary un-TAR area.
            */
            if ((mediaFormat == IMS_TAR) && (mediaType != IMS_DISK))
            {
                (void) ims_extractPath (qcSpec, stagePath);

                if ((status = ims_removeStageDir (msgDesc,
                    stagePath)) < IMS_OK)
                {
                    (void) ims_msg (msgDesc, IMS_WARNING,
                         "Could not remove the Quality Check "
                         "stage path '%s'", stagePath);
                }
            }
        }
    }

    /*
    ** Record the ending of the media job.
    */
    pnt_vdfReq->status = MEDIA_JOB_COMPLETE;
    if ((status = ims_vdfCat (pnt_vdfReq, VDF_END_JOB)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_WARNING,
            "Could not record the ending of media job '%d' in the "
            "catalog.", pnt_vdfReq->job_id);
    }

    /*
    ** Close the database server connection, free any allocated tape
    ** device, free the deviceInfo structure and shutdown the message
    ** facility.
    */
    if (connectFlag == IMS_TRUE)
    {
        (void) ims_vdfCat (pnt_vdfReq, VDF_CLOSE_CONNECTION);
    }

    (void) ims_msg (msgDesc, IMS_INFO,
        "Media distribution completed successfully.");

    if (mediaType != IMS_DISK)
    {
        if (targetPath == (char *) NULL)
        {
            (void) ims_deviceFree (msgDesc, (char *) &userSpec,
                deviceInfo->device_id);
            (void) ims_deviceTapeEject (msgDesc, deviceInfo);
        }
    }
    freeDeviceInfo (deviceInfo);
    freeItemList (itemList);
    free (pnt_vdfReq);
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    /*
    ** Close the database server connection, free any allocated tape
    ** device, free the deviceInfo structure and shutdown the message
    ** facility.
    */
    if (connectFlag == IMS_TRUE)
    {
        (void) ims_vdfCat (pnt_vdfReq, VDF_CLOSE_CONNECTION);
    }

    if (mediaType != IMS_DISK)
    {
        if (targetPath == (char *) NULL)
        {
            (void) ims_deviceFree (msgDesc, (char *) &userSpec,
                deviceInfo->device_id);
            (void) ims_deviceTapeEject (msgDesc, deviceInfo);
        }
    }
    freeDeviceInfo (deviceInfo);
    freeItemList (itemList);
    free (pnt_vdfReq);
    (void) ims_msgStructFree (msgDesc);

    exit (1);
}

/*******************************************************************
**
** runDown ()
**
** Cleanup and exit from program.
**
******************************************************************** */

static int runDown (
    int sig)
{
    /* Print out the signal caught. */
    (void) fprintf (stderr, "\n\nTermination of %s due to signal: "
        "%s (%d)\n\n", programName, ims_sigMsg (sig), sig);

    return (sig);
}

/*******************************************************************
**
** usage ()
**
** Print command line argument switches.
**
******************************************************************** */

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

    return;
}

/*******************************************************************
**
** getArgInput ()
**
** Prompt for needed information not provided in the command-line
** and command-file.
**
******************************************************************** */

static int getArgInput (
    IMS_MSG_STRUCT *msgDesc)
{
    MEDIA_ITEM_LIST *currPtr;
    MEDIA_ITEM_LIST *prevPtr;
    char inputBuffer[IMS_INPUT_BUF_LEN+1];
    char prompt[20];
    char str[1024];
    char str2[1024];
    int invalid;
    int i;
    int j;
    short itemFlag;
    int number;
    int num_items;
    int status;
    int checkStatus;
    FILE  *in_file;


    status = IMS_OK;
    prevPtr = (MEDIA_ITEM_LIST *) NULL;

    /*
    ** Prompt user for any information NOT provided in the command
    ** structure.
    */

    /* username */
    if (commands.username != (char *) NULL)
    {
        userSpec.username = commands.username;
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

        userSpec.username = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec.username, inputBuffer);
    }

    /* password */
    if (commands.password != (char *) NULL)
    {
        userSpec.password = commands.password;
    }
    else
    {
        if (ims_getPassword (inputBuffer) == NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        userSpec.password = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (userSpec.password, inputBuffer);
    }

    /* mediaType */
    if (commands.mediaType != (char *) NULL)
    {
        mediaTypeStr = commands.mediaType;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Media Type: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        mediaTypeStr = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (mediaTypeStr, inputBuffer);
    }

    /* mediaFormat */
    if (commands.mediaFormat != (char *) NULL)
    {
        mediaFormatStr = commands.mediaFormat;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Media Format: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        mediaFormatStr = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (mediaFormatStr, inputBuffer);
    }

    /* reportType */
    if (commands.reportType != (char *) NULL)
    {
        reportTypeStr = commands.reportType;
    }
    else
    {
        if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
            "Report Type: ") == (char *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Error detected while reading input string.");
            return (IMS_FATAL);
        }

        reportTypeStr = malloc (strlen (inputBuffer) + 1);
        (void) strcpy (reportTypeStr, inputBuffer);
    }

    /* orderId */
    number = 0;
    if (commands.orderId == (char *) NULL)
    {
        /* We expect a number. */
        do
        {
            if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
                "Order Id: ") == (char *) NULL)
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
                ((number < NULL_ORDER) || (number > IMS_MAX_INT)))
            {
                invalid = 1;
                (void) printf (
                    "Numerical input is not in the range of '%d' "
                    "to '%d'. Try again.\n", NULL_ORDER, IMS_MAX_INT);
            }
        }while (invalid);

        orderId = (DBINT) number;
    }
    else if (ims_isInteger (commands.orderId) == IMS_TRUE)
    {
        number = (int) atoi (commands.orderId);
        if ((number < NULL_ORDER) || (number > IMS_MAX_INT))
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The parameter 'orderId' has a value of '%d', which "
                "is not in the range of '%d' to '%d'.",
                number, NULL_ORDER, IMS_MAX_INT);
            status = IMS_ERROR;
        }
        else
        {
            orderId = (DBINT) number;
        }
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The parameter 'orderId' must contain a valid integer "
            "value.");
        status = IMS_ERROR;
    }

    /* itemCount */
    number = 0;
    if (commands.itemCount == (char *) NULL)
    {
        /* We expect a number. */
        do
        {
            if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
                "Item Count: ") == (char *) NULL)
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
                ((number < 1) || (number > IMS_MAX_SINT)))
            {
                invalid = 1;
                (void) printf (
                    "Numerical input is not in the range of '1' "
                    "to '%d'. Try again.\n", IMS_MAX_SINT);
            }
        }while (invalid);

        itemCount = (DBSMALLINT) number;
    }
    else if (ims_isInteger (commands.itemCount) == IMS_TRUE)
    {
        number = (int) atoi (commands.itemCount);
        if ((number < 1) || (number > IMS_MAX_SINT))
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The parameter 'itemCount' has a value of '%d', which "
                "is not in the range of '1' to '%d'.",
                number, IMS_MAX_SINT);
            return (IMS_ERROR);
        }
        else
        {
            itemCount = (DBSMALLINT) number;
        }
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The parameter 'itemCount' must contain a valid "
            "integer value.");
        return (IMS_ERROR);
    }

    /* itemFile */
    if (commands.itemFile  != (char *) NULL){
        /*
        **  a file is to be read to obtain the item list.  This is
        **      because if the list is too big, the command file
        **      processing doesn't work.  process each line as read.
        */
        if( (in_file = fopen (commands.itemFile, "r")) == NULL){
            (void) ims_msg( msgDesc, IMS_ERROR,
                "****** Error: can't open item file %s",
                commands.itemFile);
            return( IMS_ERROR );
        }
        num_items = 0;
        prevPtr = (MEDIA_ITEM_LIST *) NULL;
        itemFlag = IMS_TRUE;
        str[0] = '\0';
        while(  fgets( str, 1024, in_file )  !=  (char *) NULL ){
            (void) ims_truncStr( str );
            i = strlen( str );
            if(  str[i-1]  ==  '"'  )  str[i-1] = '\0';
            if(  str[0]  ==  '"'  )  str[0] = ' ';
            if(  str[0]  ==  '/'  )  str[0] = '\0'; /* comment: take out
                all the line  */

            itemFlag = 1;
            while( itemFlag ) {
                /*
                ** See if we are at the end of the list.
                */
                if ((i = ims_strIndex (str, " ")) ==  -1)
                {
                    i = strlen (str);
                    itemFlag = IMS_FALSE;
                }

                if(  i  >  0  ){
                    /*
                    ** Save our current item.
                    */
                    (void) strncpy (str2, str, i+2);
                    str2[i] = '\0';

                    /*
                    ** Position the list at the next item.
                    */
                    (void) strcpy (str, ims_truncStr (str+i));

                    if (ims_isInteger (str2) == IMS_TRUE)
                    {
                        number = (int) atoi (str2);
                    }
                    else
                    {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                            "The lines in itemFile can have /* as first"
                            " characters, double qotes optionally, and "
                            "valid integers separated by blanks.");
                        return (IMS_ERROR);
                    }

                    /*
                    ** Allocate memory space for MEDIA_ITEM_LIST.
                    */
                    if ((currPtr = (MEDIA_ITEM_LIST *) malloc
                        ((size_t) sizeof (MEDIA_ITEM_LIST))) ==
                        (MEDIA_ITEM_LIST *) NULL)
                    {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                            "Could not allocate memory for the "
                            "MEDIA_ITEM_LIST structure.");
                        return (IMS_FATAL);
                    }

                    /*
                    ** itemList points to the first element of the list.
                    */
                    if (num_items == 0)
                    {
                        itemList = currPtr;
                    }
                    else
                    {
                        prevPtr->next = currPtr;
                    }

                    num_items++;
                    currPtr->item_id = (DBSMALLINT) number;
                    currPtr->status = (DBSMALLINT) 20;
                        /* Status of LOCKED */
                    currPtr->next = (MEDIA_ITEM_LIST *) NULL;

                    prevPtr = currPtr;
                }
            }
        }

        fclose( in_file );
        /*
        ** Compare itemCount with the number of sets parsed.
        */
        if (itemCount != num_items)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The itemCount '%d' does not match the number of "
                "items in itemList '%d'.", itemCount, num_items);
            status = IMS_ERROR;
        }
    }

    /* itemList */
    if (commands.itemFile  != (char *) NULL) ;
    else  if (commands.itemList == (char *) NULL){
        for (j = 0; j < itemCount; j++){
            /* We expect a number. */
            do
            {
                (void) sprintf (prompt, "Item %d: ", j+1);

                if (ims_getString (IMS_TRUE, inputBuffer,
                    IMS_INPUT_BUF_LEN, prompt) == (char *) NULL)
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
                if((invalid == 0 ) && ((int) strlen(inputBuffer) > 0) &&
                    ((number < NULL_ORDER) || (number > IMS_MAX_SINT)))
                {
                    invalid = 1;
                    (void) printf (
                        "Numerical input is not in the range of '%d' "
                        "to '%d'. Try again.\n", NULL_ORDER,
                        IMS_MAX_SINT);
                }
            }while (invalid);

            /*
            ** Allocate memory space for the MEDIA_ITEM_LIST structure.
            */
            if ((currPtr = (MEDIA_ITEM_LIST *) malloc
                ((size_t) sizeof (MEDIA_ITEM_LIST))) ==
                (MEDIA_ITEM_LIST *) NULL)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate memory for the MEDIA_ITEM_LIST"
                    " structure.");
                return (IMS_FATAL);
            }

            /*
            ** itemList points to the first element of the list.
            */
            if (j == 0)
            {
                itemList = currPtr;
            }
            else
            {
                prevPtr->next = currPtr;
            }

            currPtr->next = (MEDIA_ITEM_LIST *) NULL;
            currPtr->item_id = (DBSMALLINT) number;
            currPtr->status = (DBSMALLINT) 20;  /* Status of LOCKED */

            prevPtr = currPtr;
        }
    }
    else  /* Parse the provided itemList. */
    {
        (void) strcpy (str, ims_truncStr (ims_removeQuotes (
            commands.itemList)));
        num_items = 0;
        prevPtr = (MEDIA_ITEM_LIST *) NULL;
        itemFlag = IMS_TRUE;
        while (itemFlag == IMS_TRUE)
        {
            /*
            ** See if we are at the end of the list.
            */
            if ((i = ims_strIndex (str, " ")) ==  -1)
            {
                i = strlen (str);
                itemFlag = IMS_FALSE;
            }

            /*
            ** Save our current item.
            */
            (void) strcpy (str2, str);
            str2[i] = '\0';

            /*
            ** Position the list at the next item.
            */
            (void) strcpy (str, ims_truncStr (str+i));

            if (ims_isInteger (str2) == IMS_TRUE)
            {
                number = (int) atoi (str2);
            }
            else
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "The parameter 'itemList' must contain valid "
                    "integer values separated by blanks.");
                return (IMS_ERROR);
            }

            /*
            ** Allocate memory space for the MEDIA_ITEM_LIST structure.
            */
            if ((currPtr = (MEDIA_ITEM_LIST *) malloc
                ((size_t) sizeof (MEDIA_ITEM_LIST))) ==
                (MEDIA_ITEM_LIST *) NULL)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not allocate memory for the "
                    "MEDIA_ITEM_LIST structure.");
                return (IMS_FATAL);
            }

            /*
            ** itemList points to the first element of the list.
            */
            if (num_items == 0)
            {
                itemList = currPtr;
            }
            else
            {
                prevPtr->next = currPtr;
            }

            num_items++;
            currPtr->item_id = (DBSMALLINT) number;
            currPtr->status = (DBSMALLINT) 20;  /* Status of LOCKED */
            currPtr->next = (MEDIA_ITEM_LIST *) NULL;

            prevPtr = currPtr;
        }

        /*
        ** Compare itemCount with the number of sets parsed.
        */
        if (itemCount != num_items)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The itemCount '%d' does not match the number of "
                "items in itemList '%d'.", itemCount, num_items);
            status = IMS_ERROR;
        }
    }

    /* targetPath */
    if (commands.targetPath != (char *) NULL)
    {
        targetPath = commands.targetPath;
    }

    /* archiveCheck */
    if (commands.archiveCheck != (char *) NULL)
    {
        archiveCheck = commands.archiveCheck;
    }

    /* server */
    if (commands.server != (char *) NULL)
    {
        userSpec.server = commands.server;
    }

    /* database */
    if (commands.database != (char *) NULL)
    {
        userSpec.database = commands.database;
    }

    /*
    ** Check the type arguments.
    */
    checkStatus = checkTypes (msgDesc);

    if (status < IMS_OK)
    {
        return (status);
    }

    if (checkStatus < IMS_OK)
    {
        return (checkStatus);
    }

    return (IMS_OK);
}


/*******************************************************************
**
** checkTypes ()
**
** Check the media type, media format and report type and
** convert it to its integer form.
**
******************************************************************** */

static int checkTypes (
    IMS_MSG_STRUCT *msgDesc)
{
    int status = IMS_OK;

    /*
    ** Check the media type.
    */
    (void) ims_toUpper (mediaTypeStr);

    if (strcmp (mediaTypeStr, "4-MM") == 0)
    {
        mediaType = IMS_4_MM;
    }
    else if (strcmp (mediaTypeStr, "4-MM HD") == 0)
    {
        mediaType = IMS_4_MM_HD;
    }
    else if (strcmp (mediaTypeStr, "8-MM") == 0)
    {
        mediaType = IMS_8_MM;
    }
    else if (strcmp (mediaTypeStr, "8-MM HD") == 0)
    {
        mediaType = IMS_8_MM_HD;
    }
    else if (strcmp (mediaTypeStr, "9-TRACK") == 0)
    {
        mediaType = IMS_9_TRACK;
    }
    else if (strcmp (mediaTypeStr, "9-TRACK HD") == 0)
    {
        mediaType = IMS_9_TRACK_HD;
    }
    else if (strcmp (mediaTypeStr, "DISK") == 0)
    {
        mediaType = IMS_DISK;
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Media type '%s' is invalid.", mediaTypeStr);
        status = IMS_ERROR;
    }

    /*
    ** Check the media format.
    */
    (void) ims_toUpper (mediaFormatStr);

    if (strcmp (mediaFormatStr, "TAR") == 0)
    {
        mediaFormat = IMS_TAR;
    }
    else if (strcmp (mediaFormatStr, "CEOS") == 0)
    {
        mediaFormat = IMS_CEOS;
    }
    else if (strcmp (mediaFormatStr, "NONE") == 0)
    {
        mediaFormat = IMS_NO_MEDIA_FORMAT;
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Media format '%s' is invalid.", mediaFormatStr);
        status = IMS_ERROR;
    }

    /*
    ** Check the report type.
    */
    (void) ims_toUpper (reportTypeStr);

    if (strcmp (reportTypeStr, "FULL") == 0)
    {
        reportType = FULL_REPORT;
    }
    else if (strcmp (reportTypeStr, "BRIEF") == 0)
    {
        reportType = BRIEF_REPORT;
    }
    else if (strcmp (reportTypeStr, "NONE") == 0)
    {
        reportType = NO_REPORT_TYPE;
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Report type '%s' is invalid.", reportTypeStr);
        status = IMS_ERROR;
    }

    if (status < IMS_OK)
    {
        return (status);
    }

    return (IMS_OK);
}

/*******************************************************************
**
** freeDeviceInfo ()
**
** Free the DEVICE_INFO structure.
**
******************************************************************** */

static void freeDeviceInfo (
    DEVICE_INFO *currPtr)
{
    DEVICE_INFO *nextPtr;

    while (currPtr != (DEVICE_INFO *) NULL)
    {
        nextPtr = currPtr->next;
        free (currPtr);
        currPtr = nextPtr;
    }

    return;
}

/*******************************************************************
**
** freeItemList ()
**
** Free the MEDIA_ITEM_LIST structure.
**
******************************************************************** */

static void freeItemList (
    MEDIA_ITEM_LIST *currPtr)
{
    MEDIA_ITEM_LIST *nextPtr;

    while (currPtr != (MEDIA_ITEM_LIST *) NULL)
    {
        nextPtr = currPtr->next;
        free (currPtr);
        currPtr = nextPtr;
    }

    return;
}
