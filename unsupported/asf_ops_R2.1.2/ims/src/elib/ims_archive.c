static char *sccs = "@(#)ims_archive.c	5.7  09/05/97";
/******************************************************************************
**
** File:        ims_archive.c
**
** Function:    Client interface routines for the IMS File Transfer
**              System (FTS).
**
** Author:      H. Sayah
**
** Modified:    6/10/93 - S. Hardman - V18.1
**              FR 71330 - Modified the getKrbTicket() function to allow
**              Kerberos service tickets to have default times greater
**              than 480 minutes (8 hours). This work was originally
**              performed under CR 4047, but management buckled and
**              assigned an FR for the change.
**
**              12/7/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files string.h, unistd.h, errno.h
**              and stdlib.h. Replaced calls to bcopy() with memcpy().
**              Replaced the use of sys_errlist[] with strerror().
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/21/94 - S. Hardman - R1B
**              Modified getEvent() to not rename the temporary files
**              until all files have been received. Also added the
**              ability to clean up temporary files if the get event
**              fails.
**
**              2/6/95 - S. Hardman - R1B
**              Added two functions freeGetList() and freeFileTypes() to
**              free allocated structures when we are finished with them.
**
**              2/21/95 - D. Crichton - R1B
**              Added processing for a new Account ID field for the request
**              structure.
**
**              3/13/95 - D. Crichton - R1B
**              Initialize getList pointer to NULL at the beginning of
**              getEvent so freeGetList will receive a NULL pointer if
**              get fails.
**
**              03/25/95 - D. Crichton - R1B
**              Move getFileTypes and getServerName processing from auxCat
**              into this file to issolate the client software from having
**              an interface with auxCat.
**
**              04/06/95 - D. Crichton - R1B
**              Modify checkFileTypes() to send an RPC to the FTS server to
**              return the list.
**
**              08/11/95 - S. Hardman - R1B
**              Modified checkRequestParam() to place a blank in the
**              request->sensor parameter if the value is null.  This allows
**              the RPC's to work correctly.
**
**              10/7/96 - D. Crichton - R2.1
**              Port to Open Client Client-Library.  Move supporting functions
**              to ims_archiveUtil.c.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_msgCb.h>
#include <ims_qi.h>
#include <ims_archive.h>
#include <ims_util.h>

#ifdef KRB
#include <ims_krb.h>
#endif  /* KRB */

#define SQL_SERVER          1
#define FTS_SERVER          2

/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_clnt.h.
** It is listed here for reference.
**
**  int ims_archive (IMS_CLNT_EVENT *);
*/

/*
** Local Functions
*/
static int getEvent (CS_COMMAND *, IMS_CLNT_EVENT *);
static int deleteEvent (CS_COMMAND *, IMS_CLNT_EVENT *);
static int addEvent (CS_COMMAND *, IMS_CLNT_EVENT *);
static int replaceEvent (CS_COMMAND *, IMS_CLNT_EVENT *);
static int whoEvent (CS_COMMAND *, IMS_CLNT_EVENT *);
static int checkRequestParam (IMS_CLNT_EVENT *);
static int validateLogin (IMS_MSG_STRUCT *, CS_COMMAND *, IMS_CLNT_EVENT *);
static void freeFileTypes (IMS_FILE_TYPES *);
static void freeGetList (IMS_GET_LIST *);
static int initContext (CS_CONTEXT **, IMS_MSG_STRUCT *);
static int openConnection (CS_CONTEXT *, CS_CONNECTION **, CS_COMMAND **,
    IMS_CLNT_EVENT *, int);
static int execCommand(IMS_MSG_STRUCT *, CS_COMMAND *, char *);
static int procResults(IMS_MSG_STRUCT *, CS_COMMAND *);
int ims_checkCtStatus ( IMS_MSG_STRUCT *, CS_COMMAND * );

#ifdef KRB
static int getKrbTicket (IMS_CLIENT_KRB *);
#endif  /* KRB */

/*
** Definition of local constants
*/

#ifdef KRB
#define SERVICE "sfoc_cdb"      /* Kerberos service name */
#endif  /* KRB */

#define BUF_SIZE 512

/*
** Error Messages
*/
static char *EINCOMPLETE_PARAM =
"Incomplete parameter for granule transfer request.  Missing: %s.";

static char *ESIZEEXCEEDED =
"Parameter length for '%s' exceeded the maximum length of '%d'.";

/*
**  used in checkRequestParam for unset parameters
*/
static char * BLANK1 = " ";
static char * EMPTY = "";

/*
** Global Variables.
*/
static char glb_ftsSrvName[IMS_COL30_LEN+1];

/******************************************************************************
**
** ims_archive ()
**
** Process an event request for the client.
**
******************************************************************************/

int ims_archive (
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    CS_COMMAND *command;
    CS_CONNECTION *connection;
    CS_CONTEXT *context;

    msgDesc = request->msgDesc;

    /*
    ** Make sure the message descriptor was allocated.
    */
    if (msgDesc == (IMS_MSG_STRUCT *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "The message facility descriptor must be allocated prior to calling ims_archive().");
        return (IMS_FATAL);
    }

    /*
    ** Check the requested parameter list from client.
    */

    if (checkRequestParam (request) < IMS_OK)
    {
        return (IMS_ERROR);
    }


    /*
    ** Initalize the context structure.
    */

    if ((status = initContext(&context, msgDesc)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Could not initialize context structure.");
        return(status);
    }

    /*
    ** If we do not have an FTS server name provided, then
    ** open the database and query for it.
    */

    if (strlen (request->ftsSrvName) == 0)
    {

        /*
        ** Open a connection to SQL server to get FTS name if
        ** not provided.
        */

        if ((status = openConnection(context, &connection,
            &command, request, SQL_SERVER)) < IMS_OK)
        {
            (void) ims_msg(msgDesc, status,
                "Could not open a connection to the SQL server.");

            (CS_VOID) ct_exit(context, CS_FORCE_EXIT);
            (CS_VOID) cs_ctx_drop(context);
            return(status);
        }

        /*
        ** Perform the query.
        */

        if ((status = ims_getServerName (msgDesc, request,
                    command)) < IMS_OK)
        {
            (void) ims_msg(msgDesc, status,
                "Could not get FTS name from the SQL server.");

            (CS_VOID) ct_exit(context, CS_FORCE_EXIT);
            (CS_VOID) cs_ctx_drop(context);
            return(status);

        }

        /*
        ** Close the connection.
        */

        (CS_VOID) ct_cancel(connection, NULL, CS_CANCEL_ALL);
        (CS_VOID) ct_cmd_drop(command);
        (CS_VOID) ct_close(connection, CS_FORCE_CLOSE);
        (CS_VOID) ct_con_drop(connection);
    }

    (void) strcpy (glb_ftsSrvName, request->ftsSrvName);


    /*
    ** Open a connection to the FTS server.
    */

    if ((status = openConnection(context, &connection,
        &command, request, FTS_SERVER)) < IMS_OK)
    {
        (void) ims_msg(msgDesc, status,
            "Could not open a connection to the FTS server.");

        (CS_VOID) ct_exit(context, CS_FORCE_EXIT);
        (CS_VOID) cs_ctx_drop(context);
        return(status);
    }

    /*
    ** Do login validation.
    */

    if ((status = validateLogin (msgDesc, command, request)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Client login validation failed.");
        goto CLOSE_CONNECTION;
    }

    /*
    ** Decode the request and take appropriate action.
    */
    switch (request->requestType)
    {
    case IMS_ADD:
        status = addEvent (command, request);
        if (status < IMS_OK)
            (void) ims_msg(msgDesc, IMS_ERROR,
                "ADD event failed.");
        break;

    case IMS_GET:
    case IMS_GET_MAX:
    case IMS_GET_LATEST:
        status = getEvent (command, request);
        if (status < IMS_OK)
            (void) ims_msg(msgDesc, IMS_ERROR,
                "GET event failed.");
        break;

    case IMS_DELETE:
        status = deleteEvent (command, request);
        if (status < IMS_OK)
            (void) ims_msg(msgDesc, IMS_ERROR,
                "DELETE event failed.");
        break;

    case IMS_REPLACE:
        status = replaceEvent (command, request);
        if (status < IMS_OK)
            (void) ims_msg(msgDesc, IMS_ERROR,
                "REPLACE event failed.");
        break;

    case IMS_WHO:
        status = whoEvent (command, request);
        if (status < IMS_OK)
            (void) ims_msg(msgDesc, IMS_ERROR,
                "WHO event failed.");
        break;

    default:
        (void) ims_msg (msgDesc, IMS_FATAL, "Invalid event type.");
        status = IMS_FATAL;
    }

    /*
    ** Close the connections.
    */
CLOSE_CONNECTION:

    (CS_VOID) ct_cancel(connection, NULL, CS_CANCEL_ALL);
    (CS_VOID) ct_cmd_drop(command);
    (CS_VOID) ct_close(connection, CS_FORCE_CLOSE);
    (CS_VOID) ct_con_drop(connection);
    (CS_VOID) ct_exit(context, CS_FORCE_EXIT);
    (CS_VOID) cs_ctx_drop(context);
    return(status);

}

/******************************************************************************
**
** getEvent ()
**
** Process any command that results in an actual transfer of a
** granule from CDB.
**
** IMS_GET, IMS_GET_MAX, IMS_GET_LATEST.
**
******************************************************************************/

static int getEvent (
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_GET_LIST *getList;
    IMS_GET_LIST *currPtr;
    IMS_GET_LIST *prevPtr;
    CS_DATAFMT datafmt[8];
    CS_RETCODE retCode;
    CS_TINYINT requestType;
    char sourceDir[IMS_PATH_LEN+1];
    char fileName[IMS_COL60_LEN+1];
    char tempFileName[IMS_COL60_LEN+1];
    char retName[IMS_COL30_LEN+1];
    char retExtension[IMS_COL10_LEN+1];
    int fileCount;
    int status;
    int len;
    int cleanUpFlag;
    int version;
    CS_INT retLastFileFlag;

    msgDesc = request->msgDesc;
    cleanUpFlag = IMS_OFF;
    getList = NULL;  /* Initialize */

    /*
    ** Validate the dataset policy for the clients request structure.
    */

    /*
    ** Check for the source directory parameter.
    */
    if ((len = strlen (ims_truncStr (request->sourceDir))) == 0)
    {
        (void) strcpy (sourceDir, ".");
    }
    else if (len > IMS_PATH_LEN)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, ESIZEEXCEEDED,
            "sourceDir", IMS_PATH_LEN);
        return (IMS_FATAL);
    }
    else
    {
        (void) strcpy (sourceDir, request->sourceDir);
    }

    /*
    ** Perform the RPC call getEventBegin.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "getEventBegin",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command getEventBegin");
        return(IMS_FATAL);
    }

    /*
    ** First bind all RPC input parameters.
    */

    /*
    ** Platform
    */

    (void) memset(&datafmt[0], 0, sizeof(CS_DATAFMT));
    datafmt[0].datatype = CS_CHAR_TYPE;
    datafmt[0].maxlength = strlen(request->platform);
    datafmt[0].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[0], (CS_VOID *)
        request->platform, strlen(request->platform), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 1 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Sensor
    */

    (void) memset(&datafmt[1], 0, sizeof(CS_DATAFMT));
    datafmt[1].datatype = CS_CHAR_TYPE;
    datafmt[1].maxlength = strlen(request->sensor);
    datafmt[1].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[1], (CS_VOID *)
        request->sensor, strlen(request->sensor), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 2 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Dataset
    */

    (void) memset(&datafmt[2], 0, sizeof(CS_DATAFMT));
    datafmt[2].datatype = CS_CHAR_TYPE;
    datafmt[2].maxlength = strlen(request->dataset);
    datafmt[2].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[2], (CS_VOID *)
        request->dataset, strlen(request->dataset), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 3 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Name
    */

    (void) memset(&datafmt[3], 0, sizeof(CS_DATAFMT));
    datafmt[3].datatype = CS_CHAR_TYPE;
    datafmt[3].maxlength = strlen(request->name);
    datafmt[3].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[3], (CS_VOID *)
        request->name, strlen(request->name), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 4 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Format
    */

    (void) memset(&datafmt[4], 0, sizeof(CS_DATAFMT));
    datafmt[4].datatype = CS_CHAR_TYPE;
    datafmt[4].maxlength = strlen(request->format);
    datafmt[4].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[4], (CS_VOID *)
        request->format, strlen(request->format), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 5 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Version
    */

    (void) memset(&datafmt[5], 0, sizeof(CS_DATAFMT));
    datafmt[5].datatype = CS_SMALLINT_TYPE;
    datafmt[5].maxlength = 2;
    datafmt[5].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[5], (CS_VOID *)
        &(request->version), 2, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 6 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Request Type Param
    */


    switch (request->requestType)
    {
        case IMS_GET:
            requestType = (CS_TINYINT) 0;
            break;

        case IMS_GET_MAX:
            requestType = (CS_TINYINT) 1;
            break;

        case IMS_GET_LATEST:
            requestType = (CS_TINYINT) 2;
            break;

        default:
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Get request type not recognized.");
            return (IMS_FATAL);
    }

    (void) memset(&datafmt[6], 0, sizeof(CS_DATAFMT));
    datafmt[6].datatype = CS_TINYINT_TYPE;
    datafmt[6].maxlength = 1;
    datafmt[6].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[6], (CS_VOID *)
        &(requestType), 1, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 7 to the FTS server.");
        return(IMS_ERROR);
    }


    /*
    ** Account Id
    */

    (void) memset(&datafmt[7], 0, sizeof(CS_DATAFMT));
    datafmt[7].datatype = CS_CHAR_TYPE;
    datafmt[7].maxlength = strlen(request->accountId);
    datafmt[7].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[7], (CS_VOID *)
        request->accountId, strlen(request->accountId), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "getEventBegin: Could not send parameter 8 to the FTS server.");
        return(IMS_ERROR);
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "getEvent: ct_send() failed.");
        return (IMS_FATAL);
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if (procResults(msgDesc, command) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
                "getEvent: Error processing the results.");
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        return (IMS_ERROR);
    }


    /*
    ** Now get all of the files for the current granule.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Granule extraction in progress for dataset '%s'.",
        request->dataset);


    retLastFileFlag = 0;
    fileCount = 0;
    prevPtr = NULL;
    while (retLastFileFlag != 1)
    {
        fileCount++;

        /*
        ** Now, get the file name from the server.
        */
        if ((status = ims_clntAcceptFileName (msgDesc, command, retName,
            retExtension, &retLastFileFlag))
            < IMS_OK)
        {
            goto GET_FAILED;
        }

        /*
        ** We will store all files with a temporary name until all
        ** files for the granule are received.  If the extraction is
        ** successful we will rename the files to their real names,
        ** otherwise we will delete the the temporary files.
        */

        /*
        ** Allocate space for the IMS_GET_LIST structure.
        **
        ** lint: pointer cast may result in improper alignment
        ** No problem, malloc() aligns on worst case boundary.
        */
        if ((currPtr = (IMS_GET_LIST *) malloc
            ((unsigned) sizeof (IMS_GET_LIST))) ==
            (IMS_GET_LIST *) NULL)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Could not allocate memory for IMS_GET_LIST structure.");
            status = IMS_FATAL;
            goto GET_FAILED;
        }

        /*
        ** getList points to the first element of the list.
        **
        ** lint: warning: variable may be used before set: prevPtr
        ** We don't use this variable until the second time through, and
        ** by that time it has been set.
        */
        if (fileCount == 1)
        {
            getList = currPtr;
        }
        else
        {
            prevPtr->next = currPtr;
        }

        /* Initialize structure members. */
        currPtr->next = (IMS_GET_LIST *) NULL;
        currPtr->fileExist = IMS_FALSE;

        /*
        ** Generate the final and temporary file names.
        */
        (void) sprintf (fileName, "%s.%s", retName, retExtension);
        ims_concatFilePath (currPtr->fullPathName, sourceDir, fileName);

        (void) sprintf (tempFileName, "%s.%d", fileName, (int) getpid());
        ims_concatFilePath (currPtr->fileToWrite, sourceDir, tempFileName);

        /*
        ** Now, get the file from the server.
        */
        if ((status=ims_clntAcceptFile (msgDesc, command,
                currPtr->fileToWrite)) < IMS_OK)
        {
            goto GET_FAILED;
        }

        /* Set the cleanUpFlag the first time through. */
        cleanUpFlag = IMS_ON;

        /* Set the previous getList pointer. */
        prevPtr = currPtr;

        (void) ims_msg (msgDesc, IMS_INFO,
            "File %s extracted from storage.", fileName);
    }

    /*
    ** File transfer operation is completed.
    ** Now rename the files to their final file names.
    */
    cleanUpFlag = IMS_OFF;
    currPtr = getList;
    while (currPtr != (IMS_GET_LIST *) NULL)
    {
        if (rename (currPtr->fileToWrite, currPtr->fullPathName) == -1)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Could not rename file '%s' to '%s'. %s",
                currPtr->fileToWrite, currPtr->fullPathName,
                strerror (errno));
            status = IMS_FATAL;
            goto GET_FAILED;
        }
        currPtr = currPtr->next;
    }

    /*
    ** Get the version number of the granules just retrieved
    ** from the server.
    */

    if (ims_getVersionNumber(msgDesc, command,
                            &version) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not get version number from FTS Server.");
        return(IMS_ERROR);

    }

    if ((request->version != version) && (request->version > 0))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Client version does not match server version");
        return(IMS_ERROR);
    }

    request->version = version;

    /*
    ** Signal the server to complete the get operation.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "getEventEnd",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command getEventBegin");
        return(IMS_FATAL);
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "getEventEnd: ct_send() failed.");
        return (IMS_FATAL);
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if (procResults(msgDesc, command) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "RPC getEventEnd failed.");
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        return (IMS_ERROR);
    }


    if (request->version == -1)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' was successfully extracted from storage.",
            retName, request->version);
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' with version '%d' was successfully extracted from storage.",
            retName, request->version);
    }

    freeGetList (getList);

    return (IMS_OK);


GET_FAILED:

    if (cleanUpFlag == IMS_ON)
    {
        currPtr = getList;
        while (currPtr != (IMS_GET_LIST *) NULL)
        {
            if (unlink (currPtr->fileToWrite) == -1)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "Could not delete file '%s'. %s",
                    currPtr->fileToWrite, strerror (errno));
            }
            currPtr = currPtr->next;
        }
    }

    (void) ims_msg (msgDesc, status,
        "Extracting granule '%s' from storage failed.",
        request->name);
    freeGetList (getList);

    return (status);
}

/******************************************************************************
**
** deleteEvent ()
**
** Request the deletion of a granule from storage.
**
******************************************************************************/

static int deleteEvent (
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    int status;
    int version;
    CS_DATAFMT datafmt[8];
    CS_RETCODE retCode;

    msgDesc = request->msgDesc;


    /*
    ** Perform the RPC call deleteEvent.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "deleteEvent",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command deleteEvent");
        return(IMS_FATAL);
    }

    /*
    ** First bind all RPC input parameters.
    */

    /*
    ** Platform
    */

    (void) memset(&datafmt[0], 0, sizeof(CS_DATAFMT));
    datafmt[0].datatype = CS_CHAR_TYPE;
    datafmt[0].maxlength = strlen(request->platform);
    datafmt[0].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[0], (CS_VOID *)
        request->platform, strlen(request->platform), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 1 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Sensor
    */

    (void) memset(&datafmt[1], 0, sizeof(CS_DATAFMT));
    datafmt[1].datatype = CS_CHAR_TYPE;
    datafmt[1].maxlength = strlen(request->sensor);
    datafmt[1].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[1], (CS_VOID *)
        request->sensor, strlen(request->sensor), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 2 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Dataset
    */

    (void) memset(&datafmt[2], 0, sizeof(CS_DATAFMT));
    datafmt[2].datatype = CS_CHAR_TYPE;
    datafmt[2].maxlength = strlen(request->dataset);
    datafmt[2].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[2], (CS_VOID *)
        request->dataset, strlen(request->dataset), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 3 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Name
    */

    (void) memset(&datafmt[3], 0, sizeof(CS_DATAFMT));
    datafmt[3].datatype = CS_CHAR_TYPE;
    datafmt[3].maxlength = strlen(request->name);
    datafmt[3].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[3], (CS_VOID *)
        request->name, strlen(request->name), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 4 to the FTS server.");
        return(IMS_ERROR);
    }


    /*
    ** Version
    */

    (void) memset(&datafmt[4], 0, sizeof(CS_DATAFMT));
    datafmt[4].datatype = CS_SMALLINT_TYPE;
    datafmt[4].maxlength = 2;
    datafmt[4].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[4], (CS_VOID *)
        &(request->version), 2, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 5 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Account Id
    */

    (void) memset(&datafmt[5], 0, sizeof(CS_DATAFMT));
    datafmt[5].datatype = CS_CHAR_TYPE;
    datafmt[5].maxlength = strlen(request->accountId);
    datafmt[5].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[5], (CS_VOID *)
        request->accountId, strlen(request->accountId), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "deleteEvent: Could not send parameter 6 to the FTS server.");
        return(IMS_ERROR);
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "deleteEvent: ct_send() failed.");
        return (IMS_FATAL);
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if (procResults(msgDesc, command) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Deleting granule '%s' from storage failed.",
            request->name);
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        return (IMS_ERROR);
    }


    /*
    ** Get the version number of the granules just deleted
    ** from the server.
    */

    if (ims_getVersionNumber(msgDesc,  command,
                            &version) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not get version number from FTS Server.");
        return(IMS_ERROR);

    }

    if ((request->version != version) && (request->version > 0))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Client version does not match server version");
        return(IMS_ERROR);
    }

    if (request->version != 0)
    {
        request->version = version;
    }

    if (request->version == -1)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' was successfully deleted from storage.",
            request->name, request->version);
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' with version '%d' was successfully deleted from storage.",
            request->name, request->version);
    }

    status = IMS_OK;
    return (status);
}


/******************************************************************************
**
** addEvent ()
**
** Request the addition of a granule to storage.
**
******************************************************************************/
static int addEvent (
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_FILE_TYPES *fileTypes;
    IMS_FILE_TYPES *currFileTypes;
    CS_DATAFMT datafmt[8];
    CS_RETCODE retCode;
    int status;
    int len;
    int extMatch;
    int metadataFlag;
    int i;
    int version;
    char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
    char sourceDir[IMS_PATH_LEN+1];
    char fileName[IMS_COL60_LEN+1];


    msgDesc = request->msgDesc;
    metadataFlag = 0;



    /*
    ** Check for the source directory parameter.
    */
    if ((len = strlen (ims_truncStr (request->sourceDir))) == 0)
    {
        (void) strcpy (sourceDir, ".");
    }
    else if (len > IMS_PATH_LEN)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, ESIZEEXCEEDED,
            "sourceDir", IMS_PATH_LEN);
        return (IMS_FATAL);
    }
    else
    {
        (void) strcpy (sourceDir, request->sourceDir);
    }

    /*
    ** Get the list of file types for the current format.
    */
    if ((fileTypes = ims_checkFileTypes(msgDesc, command , request)) ==
                (IMS_FILE_TYPES *) NULL)
    {
        return (IMS_FATAL);
    }

    /*
    ** Check to see if all of the files are accessible and readable.
    */
    currFileTypes = fileTypes;

    while (currFileTypes != (IMS_FILE_TYPES *) NULL)
    {
        /*
        ** Check if client & server local archive flags match.
        */

        if ((currFileTypes->localArchiveFlag !=
            request->localArchiveFlag) &&
            ((strcmp(currFileTypes->extension, "MTA")) ||
             (strcmp(currFileTypes->extension, "M"))))
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
               "addEvent: Client and Server local archive flags do not match");

            return(IMS_ERROR);
        }

        /* See if the policy's extensions match the requested extensions. */
        extMatch = 0;
        for (i = 0; i < (int) request->fileCount; i++)
        {
            if (strcmp (request->extensions[i],
                currFileTypes->extension) == 0)
            {
                extMatch = 1;
                break;
            }
        }

        if (extMatch != 1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The file with extension %s, is required for this granule\'s policy '%s, %s, %s, %s'.",
                currFileTypes->extension, request->platform,
                request->sensor, request->dataset,
                request->format);
            return (IMS_ERROR);
        }

        (void) sprintf (fileName, "%s.%s", request->name,
            currFileTypes->extension);

        ims_concatFilePath (fullPathName, sourceDir, fileName);

        if (access (fullPathName, R_OK) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not read file '%s'. %s.",
                fullPathName, strerror (errno));
            return (IMS_ERROR);
        }

        currFileTypes = currFileTypes->next;
    }

    /*
    ** Perform the RPC call addEventBegin.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "addEventBegin",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command addEventBegin");
        return(IMS_FATAL);
    }


    /*
    ** Platform
    */

    (void) memset(&datafmt[0], 0, sizeof(CS_DATAFMT));
    datafmt[0].datatype = CS_CHAR_TYPE;
    datafmt[0].maxlength = strlen(request->platform);
    datafmt[0].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[0], (CS_VOID *)
        request->platform, strlen(request->platform), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 1 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Sensor
    */

    (void) memset(&datafmt[1], 0, sizeof(CS_DATAFMT));
    datafmt[1].datatype = CS_CHAR_TYPE;
    datafmt[1].maxlength = strlen(request->sensor);
    datafmt[1].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[1], (CS_VOID *)
        request->sensor, strlen(request->sensor), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 2 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Dataset
    */

    (void) memset(&datafmt[2], 0, sizeof(CS_DATAFMT));
    datafmt[2].datatype = CS_CHAR_TYPE;
    datafmt[2].maxlength = strlen(request->dataset);
    datafmt[2].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[2], (CS_VOID *)
        request->dataset, strlen(request->dataset), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 3 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Name
    */

    (void) memset(&datafmt[3], 0, sizeof(CS_DATAFMT));
    datafmt[3].datatype = CS_CHAR_TYPE;
    datafmt[3].maxlength = strlen(request->name);
    datafmt[3].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[3], (CS_VOID *)
        request->name, strlen(request->name), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 4 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Format
    */

    (void) memset(&datafmt[4], 0, sizeof(CS_DATAFMT));
    datafmt[4].datatype = CS_CHAR_TYPE;
    datafmt[4].maxlength = strlen(request->format);
    datafmt[4].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[4], (CS_VOID *)
        request->format, strlen(request->format), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 5 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Account Id
    */

    (void) memset(&datafmt[5], 0, sizeof(CS_DATAFMT));
    datafmt[5].datatype = CS_CHAR_TYPE;
    datafmt[5].maxlength = strlen(request->accountId);
    datafmt[5].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[5], (CS_VOID *)
        request->accountId, strlen(request->accountId), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "addEventBegin: Could not send parameter 6 to the FTS server.");
        return(IMS_ERROR);
    }

    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "addEventBegin: ct_send() failed.");
        return (IMS_FATAL);
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if (procResults(msgDesc, command) < IMS_OK)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
                "addEventBegin: Error processing the results.");
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        status = IMS_ERROR;
        goto ADD_FAILED;
    }


    /*
    ** Transfer the files into storage.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Granule addition in progress for dataset '%s'.",
        request->dataset);

    currFileTypes = fileTypes;

    /*
    ** Loop through the file types list and send the
    ** files to the server to archive. If the file
    ** is local, then the server will verify that it can
    ** access the file while processing the metadata.  The
    ** client will not send local files.
    */

    while (currFileTypes != (IMS_FILE_TYPES *) NULL)
    {


        /*
        ** If the file is local, then go to the next
        ** file.
        */

        if ((currFileTypes->localArchiveFlag == 'Y') &&
            (currFileTypes->type != IMS_META_DATA))
        {
            (void) sprintf (fileName, "%s.%s", request->name,
                currFileTypes->extension);
            (void) ims_msg (msgDesc, IMS_INFO,
                "File %s assumed to be local.", fileName);
            currFileTypes = currFileTypes->next;
            continue;
        }

        /* Open the file to write on the server side. */
        if ((status = ims_clntOpenFile (msgDesc, command, request->name,
            currFileTypes->extension)) < IMS_OK)
        {
            goto ADD_FAILED;
        }

        (void) sprintf (fileName, "%s.%s", request->name,
            currFileTypes->extension);

        ims_concatFilePath (fullPathName, sourceDir, fileName);

        /* Send the file in chunks to the server. */
        if ((status = ims_clntSendFile (msgDesc, command,
                    fullPathName)) < IMS_OK)
        {
            goto ADD_FAILED;
        }

        /* Close the file on the server side. */
        if ((status = ims_clntCloseFile (msgDesc, command, request->name,
            currFileTypes->extension)) < IMS_OK)
        {
            goto ADD_FAILED;
        }

        /* If this is the Metadata file, process it. */
        if (currFileTypes->type == IMS_META_DATA)
        {
            /* See if we have already done this. */
            if (metadataFlag == 1)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "More than one Metadata file was encountered.");
                status = IMS_FATAL;
                goto ADD_FAILED;
            }
            else
            {
                if ((status = ims_clntValidateMetadata (msgDesc, command,
                    fileName)) < IMS_OK)
                {
                    goto ADD_FAILED;
                }
            }

            metadataFlag = 1;
        }

        (void) ims_msg (msgDesc, IMS_INFO,
            "File %s added into storage.", fileName);

        currFileTypes = currFileTypes->next;
    }

    /*
    ** Get the version number of the granules just ingested
    ** from the server.
    */

    if (ims_getVersionNumber(msgDesc, command,
                            &version) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not get version number from FTS Server.");
        return(IMS_ERROR);

    }

    request->version = version;


    /*
    ** Finish up the file transfer operation by making the end
    ** remote procedure call.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "addEventEnd",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "addEventEnd: ct_command() failed.");
        status = IMS_FATAL;
        goto ADD_FAILED;
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "addEventEnd: ct_send() failed.");
        status = IMS_FATAL;
        goto ADD_FAILED;
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if ((status = procResults(msgDesc, command)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "RPC addEventEnd failed.");

        ct_cancel(NULL, command, CS_CANCEL_ALL);
        goto ADD_FAILED;
    }


    if (request->version == -1)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' was successfully added to storage.",
            request->name, request->version);
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' with version '%d' was successfully added to storage.",
            request->name, request->version);
    }

    freeFileTypes (fileTypes);

    return (status);

    ADD_FAILED:

    (void) ims_msg (msgDesc, status,
        "Adding granule '%s' to storage failed.",
        request->name);
    freeFileTypes (fileTypes);

    return (status);
}

/******************************************************************************
**
** replaceEvent ()
**
** Request replacement of a granule in storage.
**
******************************************************************************/

static int replaceEvent (
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    IMS_FILE_TYPES *fileTypes;
    IMS_FILE_TYPES *currFileTypes;
    CS_DATAFMT datafmt[8];
    CS_RETCODE retCode;
    int status;
    int len;
    int i;
    int extMatch;
    int metadataFlag;
    char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
    char sourceDir[IMS_PATH_LEN+1];
    char fileName[IMS_COL60_LEN+1];
    int version;

    msgDesc = request->msgDesc;
    metadataFlag = 0;


    /*
    ** Check for the source directory parameter.
    */
    if ((len = strlen (ims_truncStr (request->sourceDir))) == 0)
    {
        (void) strcpy (sourceDir, ".");
    }
    else if (len > IMS_PATH_LEN)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, ESIZEEXCEEDED,
            "sourceDir", IMS_PATH_LEN);
        return (IMS_FATAL);
    }
    else
    {
        (void) strcpy (sourceDir, request->sourceDir);
    }


    /*
    ** Get the list of file types for the current format.
    */
    if ((fileTypes = ims_checkFileTypes (msgDesc, command, request)) ==
            (IMS_FILE_TYPES *) NULL)
    {
        return (IMS_FATAL);
    }

    /*
    ** Check to see if all of the files are accessible and readable.
    */
    currFileTypes = fileTypes;

    while (currFileTypes != (IMS_FILE_TYPES *) NULL)
    {
        /* See if the policy's extensions match the requested extensions. */
        extMatch = 0;
        for (i = 0; i < (int) request->fileCount; i++)
        {
            if (strcmp (request->extensions[i],
                currFileTypes->extension) == 0)
            {
                extMatch = 1;
                break;
            }
        }

        if (extMatch != 1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The file with extension %s, is required for this granule\'s policy '%s, %s, %s, %s'.",
                currFileTypes->extension, request->platform,
                request->sensor, request->dataset,
                request->format);
            return (IMS_ERROR);
        }

        (void) sprintf (fileName, "%s.%s", request->name,
            currFileTypes->extension);

        ims_concatFilePath (fullPathName, sourceDir, fileName);

        if (access (fullPathName, R_OK) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not read file '%s'. %s.",
                fullPathName, strerror (errno));
            return (IMS_ERROR);
        }

        currFileTypes = currFileTypes->next;
    }

    /*
    ** Perform the RPC call replaceEventBegin.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "replaceEventBegin",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command replaceEventBegin");
        status = IMS_FATAL;
        goto REPLACE_FAILED;
    }

    /*
    ** First bind all RPC input parameters.
    */

    /*
    ** Platform
    */

    (void) memset(&datafmt[0], 0, sizeof(CS_DATAFMT));
    datafmt[0].datatype = CS_CHAR_TYPE;
    datafmt[0].maxlength = strlen(request->platform);
    datafmt[0].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[0], (CS_VOID *)
        request->platform, strlen(request->platform), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 1 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Sensor
    */

    (void) memset(&datafmt[1], 0, sizeof(CS_DATAFMT));
    datafmt[1].datatype = CS_CHAR_TYPE;
    datafmt[1].maxlength = strlen(request->sensor);
    datafmt[1].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[1], (CS_VOID *)
        request->sensor, strlen(request->sensor), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 2 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Dataset
    */

    (void) memset(&datafmt[2], 0, sizeof(CS_DATAFMT));
    datafmt[2].datatype = CS_CHAR_TYPE;
    datafmt[2].maxlength = strlen(request->dataset);
    datafmt[2].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[2], (CS_VOID *)
        request->dataset, strlen(request->dataset), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replacEventBegin: Could not send parameter 3 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Name
    */

    (void) memset(&datafmt[3], 0, sizeof(CS_DATAFMT));
    datafmt[3].datatype = CS_CHAR_TYPE;
    datafmt[3].maxlength = strlen(request->name);
    datafmt[3].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[3], (CS_VOID *)
        request->name, strlen(request->name), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 4 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Format
    */

    (void) memset(&datafmt[4], 0, sizeof(CS_DATAFMT));
    datafmt[4].datatype = CS_CHAR_TYPE;
    datafmt[4].maxlength = strlen(request->format);
    datafmt[4].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[4], (CS_VOID *)
        request->format, strlen(request->format), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 5 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Version
    */

    (void) memset(&datafmt[5], 0, sizeof(CS_DATAFMT));
    datafmt[5].datatype = CS_SMALLINT_TYPE;
    datafmt[5].maxlength = 2;
    datafmt[5].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[5], (CS_VOID *)
        &(request->version), 2, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 6 to the FTS server.");
        status = IMS_ERROR;
        goto REPLACE_FAILED;
    }

    /*
    ** Account Id
    */

    (void) memset(&datafmt[6], 0, sizeof(CS_DATAFMT));
    datafmt[6].datatype = CS_CHAR_TYPE;
    datafmt[6].maxlength = strlen(request->accountId);
    datafmt[6].status = CS_INPUTVALUE;

    if ((retCode = ct_param(command, &datafmt[6], (CS_VOID *)
        request->accountId, strlen(request->accountId), NULL)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "replaceEventBegin: Could not send parameter 7 to the FTS server.");
        return(IMS_ERROR);
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
                "replaceEventBegin: ct_send() failed.");
        status = IMS_FATAL;
        goto REPLACE_FAILED;
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if ((status = procResults(msgDesc, command)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
                "replaceEventBegin: Error processing the results.");
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        goto REPLACE_FAILED;
    }


    /*
    ** Transfer the files into storage.
    */
    (void) ims_msg (msgDesc, IMS_INFO,
        "Granule replacement in progress for dataset '%s'.",
        request->dataset);

    currFileTypes = fileTypes;

    while (currFileTypes != (IMS_FILE_TYPES *) NULL)
    {
        /* Open the file to write on the server side. */
        if ((status = ims_clntOpenFile (msgDesc, command, request->name,
            currFileTypes->extension)) < IMS_OK)
        {
            goto REPLACE_FAILED;
        }

        (void) sprintf (fileName, "%s.%s", request->name,
            currFileTypes->extension);

        ims_concatFilePath (fullPathName, sourceDir, fileName);

        /* Send the file in chunks to the server. */
        if ((status = ims_clntSendFile (msgDesc, command,
                fullPathName)) < IMS_OK)
        {
            goto REPLACE_FAILED;
        }

        /* Close the file on the server side. */
        if ((status = ims_clntCloseFile (msgDesc, command, request->name,
            currFileTypes->extension)) < IMS_OK)
        {
            goto REPLACE_FAILED;
        }

        /* If this is the Metadata file, process it. */
        if (currFileTypes->type == IMS_META_DATA)
        {
            /* See if we have already done this. */
            if (metadataFlag == 1)
            {
                (void) ims_msg (msgDesc, IMS_FATAL,
                    "More than one Metadata file was encountered.");
                status = IMS_FATAL;
                goto REPLACE_FAILED;
            }
            else
            {
                if ((status = ims_clntValidateMetadata (msgDesc, command,
                    fileName)) < IMS_OK)
                {
                    goto REPLACE_FAILED;
                }
            }

            metadataFlag = 1;
        }

        (void) ims_msg (msgDesc, IMS_INFO,
            "File %s replaced in storage.", fileName);

        currFileTypes = currFileTypes->next;
    }

    /*
    ** Get the version number of the granules just replaced
    ** from the server.
    */

    if (ims_getVersionNumber(msgDesc, command,
                            &version) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not get version number from FTS Server.");
        return(IMS_ERROR);

    }

    if ((request->version != version) && (request->version > 0))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                "Client version does not match server version");
        return(IMS_ERROR);
    }

    request->version = version;

    /*
    ** Finish up the file transfer operation by making the end
    ** remote procedure call.
    */

    if ((retCode = ct_command(command, CS_RPC_CMD, "replaceEventEnd",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
                "replaceEventEnd: ct_command() failed.");
        status = IMS_FATAL;
        goto REPLACE_FAILED;
    }


    /*
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL, "addEventEnd: ct_send() failed.");
        status = IMS_FATAL;
        goto REPLACE_FAILED;
    }

    /*
    ** Process the RPC results.  We don't expect any returned rows.
    */

    if ((status = procResults(msgDesc, command)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "RPC replaceEventEnd failed.");

        ct_cancel(NULL, command, CS_CANCEL_ALL);
        goto REPLACE_FAILED;
    }


    if (request->version == -1)
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' was successfully replaced in storage.",
            request->name, request->version);
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_INFO,
            "Granule '%s' with version '%d' was successfully replaced in storage.",
            request->name, request->version);
    }

    freeFileTypes (fileTypes);

    return (status);

    REPLACE_FAILED:

    (void) ims_msg (msgDesc, status,
        "Replacing granule '%s' in storage failed.",
        request->name);
    freeFileTypes (fileTypes);

    return (status);
}


/******************************************************************************
**
** checkRequestParam ()
**
** Check that all required file transfer request parameters are supplied
** in the request structure.
**
******************************************************************************/

static int checkRequestParam (
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    int len;
    int i;

    msgDesc = request->msgDesc;

    /*
    ** Check that all required parameters are given in the
    ** request structure.
    */

    if (request->requestType != IMS_WHO)
    {

        /* platform */
        if ((len = strlen (ims_truncStr (request->platform))) == 0)
        {
            (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                "platform");
            return (IMS_ERROR);
        }
        if (len > IMS_COL30_LEN)
        {
            (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                "platform", IMS_COL30_LEN);
            return (IMS_ERROR);
        }

        /* sensor */
        /* The sensor may be null for certain platforms. */
        if ((len = strlen (ims_truncStr (request->sensor))) == 0)
        {
            /*
            ** The sensor may be null for certain platforms.
            ** Place a blank in the field so the RPC's function properly.
            */
            request->sensor = BLANK1;
        }
        if (len  > IMS_COL30_LEN)
        {
            (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                "sensor", IMS_COL30_LEN);
            return (IMS_ERROR);
        }

        /* dataset */
        if ((len = strlen (ims_truncStr (request->dataset))) == 0)
        {
            (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                "dataset");
            return (IMS_ERROR);
        }
        if (len > IMS_COL80_LEN)
        {
            (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                "dataset", IMS_COL80_LEN);
            return (IMS_ERROR);
        }
    }

    /* username */
    if ((len = strlen (ims_truncStr (request->username))) == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
            "username");
        return (IMS_ERROR);
    }
    if (len > IMS_COL15_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "username", IMS_COL15_LEN);
        return (IMS_ERROR);
    }

    /* password */
    if ((len = strlen (ims_truncStr (request->password))) == 0)
    {
        request->password = "";
    }
    if (len > IMS_COL15_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "password", IMS_COL15_LEN);
        return (IMS_ERROR);
    }


    /* account id */
    if ((len = strlen (ims_truncStr (request->accountId))) == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
            "accountId");
        return (IMS_ERROR);
    }
    if (len > IMS_COL15_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "accountId ", IMS_COL15_LEN);
        return (IMS_ERROR);
    }

    if (request->requestType != IMS_WHO)
    {
        /* name */
        if (request->requestType != IMS_GET_LATEST)
        {
            if ((len = strlen (ims_truncStr (request->name))) == 0)
            {
                (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                    "name");
                return (IMS_ERROR);
            }
            if (len > IMS_COL30_LEN)
            {
                (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                    "name", IMS_COL30_LEN);
                return (IMS_ERROR);
            }
        }
        else
        {
            /* Put in a dummy name for Get Latest Event messages. */
            (void) strcpy (request->name, "Latest_Available");
        }

        /* format */
        if (request->requestType != IMS_DELETE)
        {
            if ((len = strlen (ims_truncStr (request->format))) == 0)
            {
                if ((request->requestType == IMS_GET) ||
                    (request->requestType == IMS_GET_MAX) ||
                    (request->requestType == IMS_GET_LATEST))
                {
                /* Place a blank in the field so the RPC's function properly. */
                    request->format = " ";
                }
                else
                {
                    (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                        "format");
                    return (IMS_ERROR);
                }
            }
            if (len > IMS_COL10_LEN)
            {
                (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                    "format", IMS_COL10_LEN);
                return (IMS_ERROR);
            }
        }

        /* fileCount and extensions */
        if ((request->requestType == IMS_ADD) ||
            (request->requestType == IMS_REPLACE))
        {
            if (request->fileCount == 0)
            {
                (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                    "fileCount");
                return (IMS_ERROR);
            }

            for (i = 0; i < (int) request->fileCount; i++)
            {
                if ((len = strlen (ims_truncStr (request->extensions[i]))) == 0)
                {
                    (void) ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM,
                        "extensions");
                    return (IMS_ERROR);
                }
                if (len > IMS_COL10_LEN)
                {
                    (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
                        "extensions", IMS_COL10_LEN);
                    return (IMS_ERROR);
                }
            }
        }

    } /* end !IMS_WHO */

    /* programName */
    if ((len = strlen (ims_truncStr (request->programName))) == 0)
    {
        request->programName = "ims_archive";
    }

    if (len > IMS_PROGRAM_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "programName", IMS_COL30_LEN);
        return (IMS_ERROR);
    }

    /* catSrvName */
    if ((len = strlen (ims_truncStr (request->catSrvName))) == 0)
    {
        request->catSrvName = EMPTY;
    }
    if (len > IMS_NAME_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "catSrvName", IMS_COL30_LEN);
        return (IMS_ERROR);
    }

    /* catDbName */
    if ((len = strlen (ims_truncStr (request->catDbName))) == 0)
    {
        request->catDbName = EMPTY;
    }
    if (len > IMS_NAME_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "catDbName", IMS_COL30_LEN);
        return (IMS_ERROR);
    }

    /* ftsSrvName */
    if ((len = strlen (ims_truncStr (request->ftsSrvName))) == 0)
    {
        request->ftsSrvName = EMPTY;
    }
    if (len > IMS_NAME_LEN)
    {
        (void) ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED,
            "ftsSrvName", IMS_COL30_LEN);
        return (IMS_ERROR);
    }

    if ((len == 0) && (request->requestType == IMS_WHO))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "FTS server name must be provided for WHO event");
        return(IMS_ERROR);
    }

    return (IMS_OK);
}

/******************************************************************************
**
** whoEvent ()
**
** Dumps the users currently attached to the FTS server.
**
******************************************************************************/

static int whoEvent (
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    IMS_MSG_STRUCT *msgDesc;
    CS_RETCODE retCode, retFetch, retData;
    CS_INT resultType;
    int spid;
    int rowsRead;
    char event[IMS_COL30_LEN+1];
    char user[IMS_COL30_LEN+1];
    char host[IMS_COL30_LEN+1];
    char hostuser[IMS_COL30_LEN+1];
    char hostpid[IMS_COL15_LEN+1];
    int outlen;


    msgDesc = request->msgDesc;

    /*
    ** Perform the RPC call deleteEvent.
    */
    if ((retCode = ct_command(command, CS_RPC_CMD, "who",
        CS_NULLTERM, CS_NO_RECOMPILE)) != CS_SUCCEED)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not initiate the RPC command who");
        return(IMS_FATAL);
    }

    /*
    ** First bind all RPC input parameters.
    **
    ** Send the RPC to the server.
    */
    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
                "whoEvent: ct_send() failed.");
        return(IMS_FATAL);
    }

    while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
    {
        switch ((int) resultType)
        {
            case CS_ROW_RESULT:

                /*
                ** Now, get all of the results for these parameters
                ** from the server.
                */

                while (((retFetch = ct_fetch(command, CS_UNUSED,
                    CS_UNUSED, CS_UNUSED, (CS_INT *) &rowsRead)) == CS_SUCCEED) ||
                    (retFetch == CS_ROW_FAIL))
                {
                    if (retFetch == CS_ROW_FAIL)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "An error occurred fetching rows in whoEvent");
                        return(IMS_ERROR);
                    }

                    if ((retData = ct_get_data(command, 1,
                        &spid, sizeof(spid), (CS_INT *) &outlen)) != CS_END_ITEM)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "Error getting param 1 in whoEvent");
                        return(IMS_ERROR);

                    }

                    if ((retData = ct_get_data(command, 2,
                        user, sizeof(user), (CS_INT *) &outlen)) != CS_END_ITEM)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "Error getting param 2 in whoEvent");
                        return(IMS_ERROR);

                    }
                    user[outlen] = 0;


                    if ((retData = ct_get_data(command, 3,
                        event, sizeof(event), (CS_INT *) &outlen)) != CS_END_ITEM)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "Error getting param 3 in whoEvent");
                        return(IMS_ERROR);

                    }
                    event[outlen] = 0;

                    if ((retData = ct_get_data(command, 4,
                        host, sizeof(host), (CS_INT *) &outlen)) != CS_END_ITEM)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "Error getting param 4 in whoEvent");
                        return(IMS_ERROR);

                    }
                    host[outlen] = 0;

                    if ((retData = ct_get_data(command, 5,
                        hostuser, sizeof(hostuser), (CS_INT *)&outlen)) != CS_END_ITEM)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "Error getting param 5 in whoEvent");
                        return(IMS_ERROR);

                    }
                    hostuser[outlen] = 0;

                    if ((retData = ct_get_data(command, 6,
                        hostpid, sizeof(hostpid), (CS_INT *)&outlen)) != CS_END_ITEM)
                    {
                        if (retData != CS_END_DATA)
                        {
                            (void) ims_msg(msgDesc, IMS_ERROR,
                                "Error getting param 6 in whoEvent");
                            return(IMS_ERROR);
                        }

                    }
                    hostpid[outlen] = 0;


                    (void) ims_msg(msgDesc, IMS_INFO,
        "spid=%d user='%s' event='%s' machine='%s' hostuser='%s' host pid=%s",
                    spid, user, event, host, hostuser, hostpid);

                    (void) memset(user, 0, sizeof(user));
                    (void) memset(event, 0, sizeof(event));
                    (void) memset(host, 0, sizeof(host));
                    (void) memset(hostuser, 0, sizeof(hostuser));
                    (void) memset(hostpid, 0, sizeof(hostpid));


                }

                /*
                ** Check the return value of ct_fetch.
                */

                switch ((int) retFetch)
                {

                    case CS_END_DATA:
                        break;

                    case CS_FAIL:
                        (void) ims_msg(msgDesc, IMS_FATAL,
                        "Error occurred while fetching rows in whoEvent");
                        return(IMS_FATAL);

                    default:
                        (void) ims_msg(msgDesc, IMS_FATAL,
                    "Unknown error occurred fetching rows in whoEvent");
                        break;
                }


                break;

            case CS_STATUS_RESULT:
                if( ims_checkCtStatus(msgDesc, command) < IMS_OK)
                {
                    (void) ims_msg(msgDesc, IMS_ERROR,
                        "checkStatus returned an error status.");
                    return(IMS_ERROR);
                }
                break;

            case CS_PARAM_RESULT:
            case CS_CMD_SUCCEED:
            case CS_CMD_DONE:
                break;

            case CS_CMD_FAIL:
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Command  failed in whoEvent.");
                break;

            default:
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Received unepected result type of %d",
                        resultType);
                return(IMS_ERROR);

        }

    }

    /*
    ** Check the retCode parameter since it is not CS_SUCCEED.
    */

    if (retCode != CS_END_RESULTS)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "An error occurred processing the results in whoEvent");
        return(IMS_ERROR);
    }

    return(IMS_OK);

}


/******************************************************************************
**
** validateLogin ()
**
** Kerberos validation is attempted before database login validation.
**
******************************************************************************/

static int validateLogin (
    IMS_MSG_STRUCT *msgDesc,
    CS_COMMAND *command,
    IMS_CLNT_EVENT *request)
{
    int status;

#ifdef KRB
    IMS_CLIENT_KRB cKrb;

    cKrb.service = SERVICE;
    cKrb.host = request->ftsSrvNode;

    if ((status = getKrbTicket (&cKrb)) < IMS_OK)
    {
        if ((request->username == (char *) NULL) ||
            (strcmp (request->username, "") == 0))
        {
            (void) ims_msg (msgDesc, status, "Kerberos Error, %s",
                krb_err_txt[cKrb.msgIndex]);
            return (status);
        }
    }
    else  /* Send the Kerberos ticket to the server. */
    {
        if ((status = ims_clntSendKrbTicket (command, &cKrb, msgDesc))
            < IMS_OK)
        {
            (void) ims_msg (msgDesc, status,
                "Sending Kerberos ticket to server failed.");
            return (status);
        }
    }
#endif  /* KRB */

    /*
    ** Validate client login userName and password.
    */
    if ((status = ims_clntValidateLogin (msgDesc, command, request)) < IMS_OK)
    {
        return (status);
    }

    return (IMS_OK);
}

/******************************************************************************
**
** getKrbTicket ()
**
** Get a Kerberos ticket.  Variables service and host must
** be provided by the caller.  The encrypted ticket is placed
** in ktxt.dat buffer.  If an error occurs, the error number
** is returned in msgIndex.
**
** FR 71330 - Added the call to function krb_set_lifetime().  This
**            function sets the default lifetime for additional tickets
**            obtained via krb_mk_req().
**
******************************************************************************/

#ifdef KRB
static int getKrbTicket (
    IMS_CLIENT_KRB *cKrb)
{
    /*
    ** FR 71330 - Set the default ticket time to the maximum.
    ** The value 255 represents the number of 5 minute units.
    */
    (void) krb_set_lifetime (255);


    /*
    ** Make a service ticket request from the Kerberos server.
    */
    cKrb->checksum = 0;
    if ((cKrb->msgIndex = krb_mk_req (&(cKrb->ktxt), cKrb->service,
        cKrb->host, krb_realmofhost (cKrb->host),
        cKrb->checksum)) != KSUCCESS)
    {
        return (IMS_ERROR);
    }

    return (IMS_OK);
}
#endif  /* KRB */


/******************************************************************************
**
** freeFileTypes ()
**
** Free the IMS_FILE_TYPES structure used in addEvent() and replaceEvent().
**
******************************************************************************/

static void freeFileTypes (
    IMS_FILE_TYPES *currPtr)
{
    IMS_FILE_TYPES *nextPtr;

    while (currPtr != (IMS_FILE_TYPES *) NULL)
    {
        nextPtr = currPtr->next;
        (void) free ((char *) currPtr);
        currPtr = nextPtr;
    }
}


/******************************************************************************
**
** freeGetList ()
**
** Free the IMS_GET_LIST structure used in getEvent().
**
******************************************************************************/

static void freeGetList (
    IMS_GET_LIST *currPtr)
{
    IMS_GET_LIST *nextPtr;
    while (currPtr != (IMS_GET_LIST *) NULL)
    {
        nextPtr = currPtr->next;
        (void) free ((char *) currPtr);
        currPtr = nextPtr;
    }
}




/******************************************************************************
**
** openConnection ()
**
** Open a connection to the FTS server or the SQL server based on the
** requestType.
**
******************************************************************************/

static int openConnection (
    CS_CONTEXT *context,
    CS_CONNECTION **connection,
    CS_COMMAND **command,
    IMS_CLNT_EVENT *request,
    int requestType)
{
    IMS_MSG_STRUCT *msgDesc;
    CS_RETCODE retCode;
    char *catServer;
    char *catDatabase;
    char cmdBuf[IMS_COL64_LEN+1];

    /*
    ** Initialize variables.
    */
    msgDesc = request->msgDesc;

    /*
    ** Allocate a connection structure.
    */
    if ((retCode = ct_con_alloc (context, connection)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate the connection structure.");
        return (IMS_FATAL);
    }

    /*
    ** Set the CS_USERNAME property.
    */
    if ((retCode = ct_con_props (*connection, CS_SET, CS_USERNAME,
        request->username, CS_NULLTERM, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not set the username property.");
        (CS_VOID) ct_con_drop (*connection);
        return (IMS_FATAL);
    }

    /*
    ** Set the CS_PASSWORD property.
    */
    if ((retCode = ct_con_props (*connection, CS_SET, CS_PASSWORD,
        request->password, CS_NULLTERM, NULL)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not set the password property.");
        (CS_VOID) ct_con_drop (*connection);
        return (IMS_FATAL);
    }

    /*
    ** Set the CS_APPNAME property.
    */
    if ((retCode = ct_con_props (*connection, CS_SET, CS_APPNAME,
        (CS_VOID *) request->programName, CS_NULLTERM,
        NULL)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not set the application name property.");
        (CS_VOID) ct_con_drop (*connection);
        return (IMS_FATAL);
    }

    if (requestType == FTS_SERVER)
    {

#ifdef DEBUG
        (void) ims_msg (msgDesc, IMS_INFO,
            "Opening a connection to the '%s' server.",
            request->ftsSrvName);
#endif /* DEBUG */

        /*
        ** Open a Server connection.
        */
        if ((retCode = ct_connect (*connection, request->ftsSrvName,
            CS_NULLTERM)) != CS_SUCCEED)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not connect to the '%s' server.",
                request->ftsSrvName);
            (CS_VOID) ct_con_drop (*connection);
            return (IMS_ERROR);
        }
    }
    else
    {
        /*
        ** Open a connection to the database server.
        */
        if (((catServer = request->catSrvName) == NULL) ||
            (catServer[0] == '\0'))
        {
            if ((catServer = getenv("IMS_SERVER")) == NULL)
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Server name is not provided or defined in the environment.");
                return(IMS_ERROR);
            }
        }

        if (((catDatabase = request->catDbName) == NULL) ||
            (catDatabase[0] == '\0'))
        {
            if ((catDatabase = getenv("IMS_DB")) == NULL)
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Database name is not provided or defined in the environment.");
                return(IMS_ERROR);
            }
        }

#ifdef DEBUG
        (void) ims_msg (msgDesc, IMS_INFO,
            "Opening a connection to the '%s' server.",
            catServer);
#endif /* DEBUG */

        if ((retCode = ct_connect (*connection, catServer,
            CS_NULLTERM)) != CS_SUCCEED)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not connect to the '%s' server.",
                catServer);
            (CS_VOID) ct_con_drop (*connection);
            return (IMS_ERROR);
        }
    }

    /*
    ** Allocate a command structure.
    */
    if ((retCode = ct_cmd_alloc (*connection, command)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate the command structure.");
        (CS_VOID) ct_con_drop (*connection);
        return (IMS_FATAL);
    }

    if (requestType == SQL_SERVER)
    {
        /*
        ** Set the correct catalog.
        */
        (void) sprintf(cmdBuf, "use %s", catDatabase);

        if (execCommand(msgDesc, *command, cmdBuf) < IMS_OK)
        {
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Error selecting database '%s'.",
                catDatabase);
            (CS_VOID) ct_con_drop (*connection);
            return(IMS_ERROR);
        }
    }

    return (IMS_OK);
}


/******************************************************************************
**
** execCommand
**
** This function will send a command to the server, and ignore any rows
** returned.
******************************************************************************/

static int execCommand(
    IMS_MSG_STRUCT *msgDesc,
    CS_COMMAND *command,
    char *qbuf)
{

    CS_RETCODE retCode;


    /*
    ** Set command.
    */

    if ((retCode = ct_command(command, CS_LANG_CMD, qbuf,
        CS_NULLTERM, CS_UNUSED)) != CS_SUCCEED)
    {

        (void) ims_msg(msgDesc, IMS_ERROR,
            "ct_command failed in execCommand.");
        return(IMS_ERROR);
    }

    /*
    ** Send the query...
    */

    if ((retCode = ct_send(command)) != CS_SUCCEED)
    {

        (void) ims_msg(msgDesc, IMS_ERROR,
            "ct_send failed in execCommand");
        return(IMS_ERROR);
    }

    /*
    ** Process the query results.
    */

    if (procResults(msgDesc, command) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Error processing the results in execCommand");
        ct_cancel(NULL, command, CS_CANCEL_ALL);
        return(IMS_ERROR);
    }

    return(IMS_OK);
}


/******************************************************************************
**
** procResults
**
** This function will send a command to the server, and ignore any rows
** returned.
******************************************************************************/

static int procResults(
    IMS_MSG_STRUCT *msgDesc,
    CS_COMMAND *command)
{
    CS_INT resultType;
    CS_RETCODE retCode, retFetch;
    int rowsRead;


    /*
    ** Process the query results.
    */

    while ((retCode = ct_results(command, &resultType)) == CS_SUCCEED)
    {
        switch ((int) resultType)
        {
            case CS_ROW_RESULT:

                /*
                ** Now, get all of the results for these parameters
                ** from the server.
                */

                while (((retFetch = ct_fetch(command, CS_UNUSED,
                    CS_UNUSED, CS_UNUSED, (CS_INT *) &rowsRead)) == CS_SUCCEED) ||
                    (retFetch == CS_ROW_FAIL))
                {
                    if (retFetch == CS_ROW_FAIL)
                    {
                        (void) ims_msg(msgDesc, IMS_ERROR,
                            "An error occurred fetching rows in procResults");
                        return(IMS_ERROR);
                    }

                }

                /*
                ** Check the return value of ct_fetch.
                */

                switch ((int) retFetch)
                {

                    case CS_END_DATA:
                        break;

                    case CS_FAIL:
                        (void) ims_msg(msgDesc, IMS_FATAL,
                        "Error occurred while fetching rows in procResults");
                        return(IMS_FATAL);

                    default:
                        (void) ims_msg(msgDesc, IMS_FATAL,
                    "Unknown error occurred fetching rows in procResults");
                        break;
                }


                break;

            case CS_STATUS_RESULT:
                if(  ims_checkCtStatus(msgDesc, command) < IMS_OK)
                {
                    (void) ims_msg(msgDesc, IMS_ERROR,
                        "checkStatus returned an error status.");
                    return(IMS_ERROR);
                }
                break;

            case CS_PARAM_RESULT:
            case CS_CMD_SUCCEED:
            case CS_CMD_DONE:
                break;

            case CS_CMD_FAIL:
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Command  failed in procResults.");
                break;

            default:
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Received unepected result type of %d",
                        resultType);
                return(IMS_ERROR);

        }

    }

    /*
    ** Check the retCode parameter since it is not CS_SUCCEED.
    */

    if (retCode != CS_END_RESULTS)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "An error occurred processing the results in procResults");
        return(IMS_ERROR);
    }
    (CS_VOID) ct_cancel(NULL, command, CS_CANCEL_ALL);

    return(IMS_OK);
}


/******************************************************************************
**
** initContext ()
**
******************************************************************************/

static int initContext (
    CS_CONTEXT **context,
    IMS_MSG_STRUCT *msgDesc)
{
    CS_RETCODE retCode;

    /*
    ** Allocate the context structure.
    */
    if ((retCode = cs_ctx_alloc (CS_VERSION_100, context)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate the context structure.");
        return (IMS_FATAL);
    }

    /*
    ** Initialize the client library.
    */
    if ((retCode = ct_init (*context, CS_VERSION_100)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not initialize the client library.");
        (CS_VOID) cs_ctx_drop (*context);
        return (IMS_FATAL);
    }

    /*
    ** Install the client message handler.
    */
    if ((retCode = ct_callback (*context, NULL, CS_SET, CS_CLIENTMSG_CB,
        (CS_VOID *) ims_msgClientCb)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not install the client message handler.");
        (CS_VOID) ct_exit (*context, CS_FORCE_EXIT);
        (CS_VOID) cs_ctx_drop (*context);
        return (IMS_FATAL);
    }

    /*
    ** Install the server message handler.
    */
    if ((retCode = ct_callback (*context, NULL, CS_SET, CS_SERVERMSG_CB,
        (CS_VOID *) ims_msgServerCb)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not install the server message handler.");
        (CS_VOID) ct_exit (*context, CS_FORCE_EXIT);
        (CS_VOID) cs_ctx_drop (*context);
        return (IMS_FATAL);
    }


    /*
    ** Save our message descriptor for use in message callbacks.
    */
    if ((retCode = cs_config (*context, CS_SET, CS_USERDATA,
        (CS_VOID *) &msgDesc, (CS_INT) sizeof (msgDesc),
        (CS_INT *) NULL)) != CS_SUCCEED)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not save the message descriptor.");
        (CS_VOID) ct_exit (*context, CS_FORCE_EXIT);
        (CS_VOID) cs_ctx_drop (*context);
        return (IMS_FATAL);
    }

    return (IMS_OK);
}


