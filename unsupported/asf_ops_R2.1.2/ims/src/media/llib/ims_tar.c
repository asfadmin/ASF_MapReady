static char *sccs = "@(#)ims_tar.c	5.5  03/07/97";
/******************************************************************************
**
** File:        ims_tar.c
**
** Function:    API routines for building a tar file or FTPing Files.
**
** Author:      Dan Crichton
**
** Date:        8/21/95
**
** Modification:
**              12/1/95 D. Crichton R1B'
**              Added ability to FTP and Copy files to remote sites
**              or directories.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <ims_query.h>
#include <ims_media.h>
#include <ims_tar.h>
#include <ims_childHandler.h>
#include <ims_util.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_tar.h.
** They are listed here for reference.
**
** int ims_sendRemoteFiles(IMS_MSG_STRUCT *, char *, char *, char *, int,
**   MEDIA_ITEM_LIST *, pnt_vdf_cat_request_t);
** int ims_formTapeTAR (IMS_MSG_STRUCT *, char *, char *, char *, int, int,
**   DBSMALLINT [], pnt_vdf_cat_request_t);
** int ims_removeStageDir (IMS_MSG_STRUCT *, char *);
** int ims_unTarfiles (IMS_MSG_STRUCT *, char *, char *);
*/

/*
** Local Functions.
*/
static int tarFiles(IMS_MSG_STRUCT *, IMS_TAR_LIST *, char *, char *);
static void free_tarList(IMS_TAR_LIST *);
static int copyFiles(IMS_MSG_STRUCT *, IMS_TAR_LIST *, char *);

/******************************************************************************
**
** tarFiles
**
******************************************************************************/

int tarFiles (
    IMS_MSG_STRUCT *msgDesc,
    IMS_TAR_LIST *tarlist,
    char *argument,
    char *tarName)

{
    char **arg;
    IMS_TAR_LIST *ptr;
    int i, pid;
    int count;
    int status;
    char *taskName = "/usr/bin/tar";
    char *changeDir = "-C";
    char tempName[IMS_PATH_LEN];


    /*
    ** Count the number of files;
    */
    ptr = tarlist;
    count = 0;

    while (ptr != NULL)
    {
        count++;
        ptr = ptr->next;
    }

    /*
    ** Allocate memory for argument list.
    ** Each file needs to allocate room for a path, filename
    ** and the -C option to change to that dir.
    */

    arg = (char **) malloc(sizeof(char *) * (count * 3 + 4));

    if (arg == NULL)
    {
        (void) ims_msg(msgDesc, IMS_FATAL,
            "Could not allocate memory for argument list.");
        return(IMS_FATAL);
    }

    /*
    ** Setup arguments
    */

    *(arg + 1) = argument;  /* Tar options */
    *(arg + 2) = tarName;

    for (i = 3, ptr = tarlist; ptr != NULL; i++, ptr = ptr->next)
    {
        *(arg + i) = (char *) changeDir;
        i++;
        *(arg + i) = (char *) ptr->path;
        i++;

        /*
        ** Make sure that only the path is sent to TAR.
        */

        *(arg + i) = (char *) ims_extractFileName(ptr->filename);

        /*
        ** Check access permissions to file
        */


        sprintf(tempName, "%s/%s", ptr->path,
                    (char *) ims_extractFileName(ptr->filename));

        if (access(tempName, R_OK) < 0)
        {
            (void) ims_msg(msgDesc, IMS_ERROR,
                "TAR does not have access to read file %s\n", tempName);
            return(IMS_ERROR);
        }

    }
    arg[i] = NULL;  /* NULL pointer for end of argument list. */


    /*
    ** Fork and execute the report processor.
    */

    *arg = (char *) taskName;

    pid = fork();

    if (pid < 0)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not fork a tar process.");
        return(IMS_ERROR);
    }
    else if (pid == 0)
    {
        /*
        ** Child process
        */
        (void) execv(taskName, arg);
        exit(-1);  /* This exit should not occur unless task is not found. */
    }

#ifdef DEBUG
    (void) ims_msg (msgDesc, IMS_INFO,
        "Started process '%d' for image '%s'.", pid, taskName);
#endif  /* DEBUG */

    /*
    ** Check exit status of process.
    */
    if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The tar process failed.");
        return (status);
    }

    return(IMS_OK);
}


/******************************************************************************
**
** ims_formTapeTAR ()
**
******************************************************************************/

int ims_formTapeTAR (
    IMS_MSG_STRUCT *msgDesc,
    char *vdf_name,
    char *ndf_name,
    char *file_name,
    int orderId,
    int itemCount,
    DBSMALLINT items[],
    pnt_vdf_cat_request_t pnt_vdfReq)
{
    int i, x;
    pnt_product_t pnt_prod;         /* Ptr to product structure/ */
    IMS_TAR_LIST *ptr, *tar_list;
    int status;
    int count;
    char tempPath[IMS_PATH_LEN];
    char tempFile[IMS_PATH_LEN];

    (void) ims_msg (msgDesc, IMS_INFO,
        "Started TAR formatted distribution for order %d, path '%s'.",
        orderId, file_name);

    if (((strlen(vdf_name) == 0) && (strlen(ndf_name) != 0))  ||
        ((strlen(vdf_name) != 0) && (strlen(ndf_name) == 0)))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "VDF or NDF name has no value.");
        return(IMS_ERROR);
    }



    /*
    ** Initialize product and vdf cat request structures
    */
    pnt_prod = (pnt_product_t) malloc(sizeof(product_t));
    pnt_vdfReq->pnt_save_prod = pnt_prod;
    pnt_prod->order_id = orderId;

    /*
    ** Get the items to put in the tar file using the itemArray.
    */

    count = 0;

    for (i = 0; i < itemCount; i++)
    {
        /*
        ** Setup product structure to perform cat function.
        */
        pnt_prod->item_id = items[i];
        pnt_vdfReq->maxlen = 0;

        /*
        ** Get the product path.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_PATH);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get path for data file.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain the granule path for item '%d' of order '%d'.",
                items[i], orderId);
            return(IMS_ERROR);
        }

        /*
        ** Now, get the file name.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_FILE_NAME);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get name for data file.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain the granule name for item '%d' of order '%d'.",
                items[i], orderId);
            return(IMS_ERROR);
        }

        /*
        ** Check the status of the granule.
        */
        if (pnt_prod->status != GRANULE_AVAILABLE)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Granule '%s', associated with item '%d' of order '%d', does "
                "not have a status of 'Available'.",
                pnt_prod->name, pnt_prod->item_id, pnt_prod->order_id);
            return (IMS_ERROR);
        }

        /*
        ** Check the version of the granule.
        ** We do not support distributing internally versioned products.
        */
        if (pnt_prod->version != NO_VERSION)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Granule '%s', associated with item '%d' of order '%d', uses "
                "internal versioning which is not supported.",
                pnt_prod->name, pnt_prod->item_id, pnt_prod->order_id);
            return (IMS_ERROR);
        }

        /*
        ** Now, get the file extension list.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_EXT_LIST);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get extension list.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain an extension list for granule '%s' "
                "with a format of '%s'.",
                pnt_prod->name, pnt_prod->format);
            return(IMS_ERROR);
        }


        /*
        ** Okay, we have the path now lets add it to our file list.
        */
        for (x = 0; x < pnt_prod->extCount; x++)
        {

            count++;

            if (count == 1)
            {
                /*
                ** Create header node for linked list
                */
                tar_list = (void *) malloc(sizeof(IMS_TAR_LIST));
                ptr = tar_list;
            }
            else
            {
                /*
                ** Create trailer nodes for linked list.
                */
                ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
                ptr = ptr->next;
            }

            /*
            ** Check that memory was allocated.
            */

            if (ptr == NULL)
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                        "Could not allocate memory for tar list");
                return(IMS_ERROR);
            }

            ptr->next = NULL;

            ptr->filename = ptr->path = NULL;

            /*
            ** Size = path len + product length + \ + extension + null
            */
            ptr->path = (char *) malloc( 1 + strlen(pnt_vdfReq->path_name));

            ptr->filename = (char *)
                    malloc(strlen(pnt_prod->name) + 7);
            /*
            ** Check that memory was allocated for filename.
            */

            if ((ptr->filename == NULL) || (ptr->path == NULL))
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for tar filename");
                free_tarList(tar_list);
                free(pnt_prod);
                return(IMS_ERROR);
            }

            (void) sprintf(ptr->filename, "%s.%s",
                        pnt_prod->name, pnt_prod->extArray[x]);
            (void) strcpy(ptr->path, pnt_vdfReq->path_name);

        } /* end for */


    }

    /*
    ** Free the pnt_product since we are done with the catalog
    ** portion.
    */

    free(pnt_prod);


    if ((strlen(vdf_name) == 0) && (strlen(ndf_name) == 0))
        goto skip_ceos_files;


    /*
    **  Add VDF File to archive file list
    */

    if (itemCount == 0)
    {
        /*
        ** Create header node for linked list
        */
        tar_list = (void *) malloc(sizeof(IMS_TAR_LIST));
        ptr = tar_list;

    }
    else
    {
        /*
        ** Create trailer nodes for linked list.
        */
        ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
        ptr = ptr->next;
    }

    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not allocate pointer for list");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }


    /*
    ** Add NULL VDF to TAR archive
    */


    if (ims_extractPath(vdf_name, tempPath) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not extract path for VDF file.");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }

    if (ims_extractFileName(vdf_name) == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not extract filename for VDF file.");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }

    (void) strcpy(tempFile, ims_extractFileName(vdf_name));

    ptr->path = (char *) malloc(strlen(tempPath) + 1);
    ptr->filename = (char *) malloc(strlen(tempFile) + 1);

    if (ptr->path == NULL || ptr->filename == NULL)
    {
            (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for tar filename");
    }

    (void) strcpy(ptr->filename, tempFile);
    (void) strcpy(ptr->path, tempPath);

    /*
    ** Add Null NDF File to TAR Archive List
    */

    ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
    ptr = ptr->next;
    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not allocate pointer for list");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }
    ptr->next = NULL;

    if (ims_extractPath(ndf_name, tempPath) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not extract path for NDF file.");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }

    if (ims_extractFileName(ndf_name) == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not extract filename for NDF file.");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }

    strcpy(tempFile, (char *) ims_extractFileName(ndf_name));
    ptr->path = (char *) malloc(strlen(tempPath) + 1);
    ptr->filename = (char *) malloc(strlen(tempFile) + 1);

    if (ptr->path == NULL || ptr->filename == NULL)
    {
            (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for tar filename");
    }

    (void) strcpy(ptr->filename, tempFile);
    (void) strcpy(ptr->path, tempPath);


skip_ceos_files:

    /*
    ** Tar off files
    */

    if (tarFiles(msgDesc, tar_list, "cef", file_name) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not create tar archive");
        free_tarList(tar_list);
        return(IMS_ERROR);
    }

    /*
    ** Free it all
    */
    free_tarList(tar_list);

    (void) ims_msg (msgDesc, IMS_INFO,
        "Completed TAR formatted distribution for '%s'.", file_name);

    return(IMS_OK);
}


/******************************************************************************
**
** free_tarList ()
**
******************************************************************************/

void free_tarList(IMS_TAR_LIST *tar_list)
{
    IMS_TAR_LIST *ptr;

    while (tar_list != NULL)
    {
        if (tar_list->filename != NULL)
            free(tar_list->filename);
        if (tar_list->path != NULL)
            free(tar_list->path);
        ptr = tar_list;
        tar_list = tar_list->next;
        free(ptr);
    }

}


/******************************************************************************
**
** ims_unTarFiles ()
**
** Extracts files from the TAR archive and places them into
** a staging area.
**
******************************************************************************/

int ims_unTarfiles (
    IMS_MSG_STRUCT *msgDesc,
    char *targetPath,
    char *destPath)
{
    int status;
    pid_t pid;
    char currentPath[IMS_PATH_LEN+1];
    char *taskName = "/usr/bin/tar";
    char *taskArgs = "xfe";

    /*
    ** Get the current working directory.
    */
    if ((char *) getcwd (currentPath, IMS_PATH_LEN+2) == (char *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not get the current working directory. %s",
            strerror (errno));
        return (IMS_ERROR);
    }

    /*
    ** Change the directory to the stage area.
    */
    if ((status = chdir (destPath)) == -1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not change directory to stage area '%s'. %s",
            destPath, strerror (errno));
        return (IMS_ERROR);
    }

    /*
    ** Start the child process.
    */
    if ((pid = ims_startChild (msgDesc, taskName, taskName,
        taskArgs, targetPath, (char *) 0)) == -1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not start the un-tar process.");
        (void) chdir (currentPath);
        return (IMS_ERROR);
    }

    /*
    ** Wait for the child process to complete.
    */
    if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The un-tar process failed.");
        (void) chdir (currentPath);
        return (status);
    }

    (void) chdir (currentPath);

    return (IMS_OK);
}

/******************************************************************************
**
** copyFiles ()
**
******************************************************************************/

static int copyFiles (
    IMS_MSG_STRUCT *msgDesc,
    IMS_TAR_LIST *copylist,
    char *stageSpec)

{
    IMS_TAR_LIST *ptr;
    int pid;
    int status;
    char *taskName = "/usr/bin/cp";
    char tempName[IMS_PATH_LEN];
    char targetName[IMS_PATH_LEN];


    /*
    ** Copy each file independently
    */

    ptr = copylist;

    while (ptr != NULL)
    {
        (void)  ims_msg( msgDesc, IMS_INFO,
            "Copying file %s to FTP stage area.", ptr->filename );

        /*
        ** Build name
        */
        (void) sprintf(tempName, "%s/%s",
            ptr->path, ims_extractFileName(ptr->filename));

        /*
        ** Start child
        */
        if ((pid = ims_startChild(msgDesc, taskName, taskName,
                    tempName, stageSpec, (char *) 0)) == -1)
        {
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not start the copy process for file '%s.",
                tempName);

            /*
            ** Set item status
            */
            if (ptr->itemPtr != NULL)
            {
                ptr->itemPtr->status = MEDIA_ERROR;
            }

            return (IMS_ERROR);
        }

        /*
        ** Wait for child to finish
        */
        if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, status,
                "The copy process failed.");

            /*
            ** Set item status
            */
            if (ptr->itemPtr != NULL)
            {
                ptr->itemPtr->status = MEDIA_ERROR;
            }

            return (status);
        }

        /*
        ** Make sure filename has correct permissions.
        */

        (void) sprintf(targetName, "%s/%s", stageSpec,
                ims_extractFileName(ptr->filename));

        (void) chmod(targetName, 0664);
        ptr = ptr->next;
    }

    return(IMS_OK);
}

/******************************************************************************
**
** ims_sendRemoteFiles ()
**
******************************************************************************/

int ims_sendRemoteFiles (
    IMS_MSG_STRUCT *msgDesc,
    char *vdf_name,
    char *ndf_name,
    char *stageSpec,
    int orderId,
    MEDIA_ITEM_LIST *itemList,
    pnt_vdf_cat_request_t pnt_vdfReq)
{
    MEDIA_ITEM_LIST *itemPtr;
    int x;
    pnt_product_t pnt_prod;         /* Ptr to product structure */
    IMS_TAR_LIST *ptr, *ftp_list;
    int status;
    int count;
    char tempPath[IMS_PATH_LEN];
    char tempFile[IMS_PATH_LEN];


    (void) ims_msg (msgDesc, IMS_INFO,
        "Started FTP distribution for '%s'.", stageSpec);

    if (((strlen(vdf_name) == 0) && (strlen(ndf_name) != 0))  ||
        ((strlen(vdf_name) != 0) && (strlen(ndf_name) == 0)))
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "VDF or NDF name has no value.");
        return(IMS_ERROR);
    }

    /*
    ** Initialize product and vdf cat request structures
    */
    pnt_prod = (pnt_product_t) malloc(sizeof(product_t));
    (void) memset (pnt_prod, 0, (size_t) sizeof (pnt_product_t));
    pnt_vdfReq->pnt_save_prod = pnt_prod;
    pnt_prod->order_id = orderId;

    count = 0;


    itemPtr = itemList;
    while (itemPtr != (MEDIA_ITEM_LIST *) NULL)
    {

        /*
        ** Setup product struct to perform cat functions
        */

        pnt_prod->item_id = itemPtr->item_id;
        pnt_vdfReq->maxlen = 0;

        /*
        ** Get the product path.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_PATH);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get path for data file.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain the granule path for item '%d' of order '%d'.",
                itemPtr->item_id, orderId);
            return(IMS_ERROR);
        }

        /*
        ** Now, get the file name.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_FILE_NAME);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get name for data file.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain the granule name for item '%d' of order '%d'.",
                itemPtr->item_id, orderId);
            return(IMS_ERROR);
        }

        /*
        ** Check the status of the granule.
        */
        if (pnt_prod->status != GRANULE_AVAILABLE)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Granule '%s', associated with item '%d' of order '%d', does "
                "not have a status of 'Available'.",
                pnt_prod->name, pnt_prod->item_id, pnt_prod->order_id);
            return (IMS_ERROR);
        }

        /*
        ** Check the version of the granule.
        ** We do not support distributing internally versioned products.
        */
        if (pnt_prod->version != NO_VERSION)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Granule '%s', associated with item '%d' of order '%d', uses "
                "internal versioning which is not supported.",
                pnt_prod->name, pnt_prod->item_id, pnt_prod->order_id);
            return (IMS_ERROR);
        }

        /*
        ** Now, get the file extension list.
        */
        status = ims_vdfCat(pnt_vdfReq, VDF_GET_EXT_LIST);

        if (pnt_vdfReq->no_data || status < IMS_OK)
        {
            /*
            ** Could not get extension list.
            */
            (void) ims_msg(msgDesc, IMS_ERROR,
                "Could not obtain an extension list for granule '%s' "
                "with a format of '%s'.",
                pnt_prod->name, pnt_prod->format);
            return(IMS_ERROR);
        }

        /*
        ** Okay, we have the path now lets add it to our file list.
        */
        for (x = 0; x < pnt_prod->extCount; x++)
        {
            count++;

            if (count == 1)
            {
                /*
                ** Create header node for linked list
                */
                ftp_list = (void *) malloc(sizeof(IMS_TAR_LIST));
                ptr = ftp_list;
            }
            else
            {
                /*
                ** Create trailer nodes for linked list.
                */
                ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
                ptr = ptr->next;
            }

            /*
            ** Check that memory was allocated.
            */

            if (ptr == NULL)
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                        "Could not allocate memory for ftp list");

                return(IMS_ERROR);
            }

            ptr->next = NULL;

            ptr->filename = ptr->path = NULL;

            ptr->itemPtr = itemPtr;

            /*
            ** Size = path len + product length + \ + extension + null
            */
            ptr->path = (char *) malloc( 1 + strlen(pnt_vdfReq->path_name));

            ptr->filename = (char *)
                    malloc(strlen(pnt_prod->name) + 7);
            /*
            ** Check that memory was allocated for filename.
            */

            if ((ptr->filename == NULL) || (ptr->path == NULL))
            {
                (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for ftp filename");
                free_tarList(ftp_list);
                free(pnt_prod);
                return(IMS_ERROR);
            }

            (void) sprintf(ptr->filename, "%s.%s",
                        pnt_prod->name, pnt_prod->extArray[x]);
            (void) strcpy(ptr->path, pnt_vdfReq->path_name);

        } /* end for */

        itemPtr = itemPtr->next;

    }

    /*
    ** Free the pnt_product since we are done with the catalog
    ** portion.
    */

    free (pnt_prod);

    if ((strlen(vdf_name) == 0) && (strlen(ndf_name) == 0))
        goto skip_ceos_files;

    /*
    **  Add VDF File to archive file list
    */

    if (ptr == NULL)
    {
        /*
        ** Create header node for linked list
        */
        ftp_list = (void *) malloc(sizeof(IMS_TAR_LIST));
        ptr = ftp_list;

    }
    else
    {
        /*
        ** Create trailer nodes for linked list.
        */
        ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
        ptr = ptr->next;
    }

    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not allocate pointer for list");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    ptr->itemPtr = NULL; /* No item for VDF in itemList */

    /*
    ** Add NULL VDF to FTP archive
    */

    if (ims_extractPath(vdf_name, tempPath) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not extract path for VDF file.");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    if (ims_extractFileName(vdf_name) == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not extract filename for VDF file.");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    (void) strcpy (tempFile, ims_extractFileName (vdf_name));

    ptr->path = (char *) malloc(strlen(tempPath) + 1);
    ptr->filename = (char *) malloc(strlen(tempFile) + 1);

    if (ptr->path == NULL || ptr->filename == NULL)
    {
            (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for ftp filename");
    }

    (void) strcpy(ptr->filename, tempFile);
    (void) strcpy(ptr->path, tempPath);

    /*
    ** Add Null NDF File to FTP Archive List
    */

    ptr->next = (void *) malloc(sizeof(IMS_TAR_LIST));
    ptr = ptr->next;
    if (ptr == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not allocate pointer for list");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    ptr->next = NULL;
    ptr->itemPtr = NULL; /* No item for NDF in itemList */

    if (ims_extractPath(ndf_name, tempPath) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR, "Could not extract path for NDF file.");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    if (ims_extractFileName(ndf_name) == NULL)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not extract filename for NDF file.");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    (void) strcpy(tempFile, ims_extractFileName(ndf_name));
    ptr->path = (char *) malloc(strlen(tempPath) + 1);
    ptr->filename = (char *) malloc(strlen(tempFile) + 1);

    if (ptr->path == NULL || ptr->filename == NULL)
    {
            (void) ims_msg(msgDesc, IMS_ERROR,
                    "Could not allocate memory for ftp filename");
    }

    (void) strcpy(ptr->filename, tempFile);
    (void) strcpy(ptr->path, tempPath);


skip_ceos_files:

    /*
    ** Copy or FTP off files
    */

    if (copyFiles(msgDesc, ftp_list,  stageSpec) < IMS_OK)
    {
        (void) ims_msg(msgDesc, IMS_ERROR,
            "Could not copy files to staging area.");
        free_tarList(ftp_list);
        return(IMS_ERROR);
    }

    /*
    ** Free it all
    */
    free_tarList(ftp_list);

    (void) ims_msg (msgDesc, IMS_INFO,
        "Completed FTP distribution for '%s'.", stageSpec);

    return(IMS_OK);
} /* Copy Files */


/******************************************************************************
**
** ims_removeStageDir ()
**
** Removes a directory in the staging area once the directory and
** subsequent files are no longer needed.
**
******************************************************************************/

ims_removeStageDir (
    IMS_MSG_STRUCT *msgDesc,
    char *targetPath)
{
    char *taskName = "/usr/bin/rm";
    char *taskArgs = "-r";

    int status;
    pid_t pid;

    /*
    ** Start the child process.
    */
    if ((pid = ims_startChild (msgDesc, taskName, taskName,
        taskArgs, targetPath, (char *) 0)) == -1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not start the rm process.");
        return (IMS_ERROR);
    }

    /*
    ** Wait for the child process to complete.
    */
    if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "The rm process failed.");
        return (IMS_ERROR);
    }

    return (IMS_OK);
}

