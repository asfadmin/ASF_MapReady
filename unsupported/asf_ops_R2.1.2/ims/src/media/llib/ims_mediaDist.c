static char *sccs = "@(#)ims_mediaDist.c	5.6  03/07/97";
/***************************************************************
**
** File:        ims_mediaDist.c
**
** Function:    Routine to generate and distribute products onto media.
**
** Author:      S. Hardman
**
** Date:        6/14/95
**
** Modified:    4/19/96 - D. Crichton - R1B'
**                    Change mkdir() to be 0770 permission.
**
** Modified:    1/23/97 - D. Pass - R2.1
**                    Added option to discontinue tape size check.
**                      The reportType is >= 100 in this case.
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_tar.h>
#include <ims_util.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_media.h.
** They are listed here for reference.
**
**  int ims_mediaDist (IMS_MSG_STRUCT *, char *, DBSMALLINT, DBSMALLINT,
**      DBINT, MEDIA_ITEM_LIST *, DEVICE_INFO *, char *, int, int *);
*/

/*
** Local Functions.
*/
static int checkMediaType (IMS_MSG_STRUCT *, char *, DBSMALLINT);
static void removeStageFiles (char *, char *);

/*
** External Functions. (Put in a header file.)
*/
int GetProdList (IMS_MSG_STRUCT *, DBINT, DBSMALLINT *, int, int *,
    pnt_vdf_cat_request_t, pnt_product_t *);
int ims_formTapeCEOS (char *, char *, DBINT, int, DBSMALLINT *, int, int,
    char *, pnt_vdf_cat_request_t, IMS_MSG_STRUCT *);
int MediaFit (DBINT, pnt_product_t, pnt_product_t, int,
    pnt_vdf_cat_request_t, IMS_MSG_STRUCT *, int );
int create_null_vdf (char *, char *, short, IMS_MSG_STRUCT *);

/******************************************************************************
**
** ims_mediaDist ()
**
******************************************************************************/

int ims_mediaDist (
    IMS_MSG_STRUCT *msgDesc,
    pnt_vdf_cat_request_t pnt_vdfReq,
    DBSMALLINT mediaType,
    DBSMALLINT mediaFormat,
    DBINT orderId,
    MEDIA_ITEM_LIST *itemList,
    DEVICE_INFO *deviceInfo,
    char *qcSpec,
    int reportType,
    int *ceosFlag)
{
    MEDIA_ITEM_LIST *itemPtr;
    DBSMALLINT itemArray[9000];  /* ???? Think about this. */
    pnt_product_t pnt_first_prod;
    pnt_product_t pnt_curr_prod;
    pnt_product_t pnt_first_dig_prod;
    pnt_product_t pnt_last_dig_prod;
    char vdfSpec[IMS_PATH_LEN+1];
    char ndfSpec[IMS_PATH_LEN+1];
    char stageSpec[IMS_PATH_LEN+1];
    int status;
    int itemCount;
    int debug_print;
    int dump_print;
    int errorFlag;
    long strLoc;
    int  no_size_check; /*  if true, size check not done */

#ifdef DEBUG
    debug_print = IMS_TRUE;
#else
    debug_print = IMS_FALSE;
#endif  /* DEBUG */

#ifdef DUMP
    dump_print = IMS_TRUE;
#else
    dump_print = IMS_FALSE;
#endif  /* DUMP */

    /* ************************************
    **  New option: (1/23/97)  if report_type >=  100,
    **      then do not use the tape size check
    */
    no_size_check = IMS_FALSE;
    if(  reportType  >=  100  ){
        reportType -= 100;
        no_size_check = IMS_TRUE;
    }

    /*
    ** Populate itemArray and determine itemCount.
    */
    itemCount = 0;
    itemPtr = itemList;
    while (itemPtr != (MEDIA_ITEM_LIST *) NULL)
    {
        itemArray[itemCount] = itemPtr->item_id;
        itemCount++;
        itemPtr = itemPtr->next;
    }

    if (itemCount == 0)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "There are no items in the item list.");
        free (pnt_vdfReq);
        return (IMS_ERROR);
    }

    /*
    ** Get the product info and put it in a linked list.
    */
    pnt_first_prod = NULL;
    if ((status = GetProdList (msgDesc, orderId, itemArray,
        itemCount, ceosFlag, pnt_vdfReq, &pnt_first_prod)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not obtain the product list.");
        goto ERROR;
    }

    /*
    ** Check the mediacode for each product in the list
    ** against the given media type.
    */
    errorFlag = IMS_FALSE;
    pnt_curr_prod = pnt_first_dig_prod = pnt_first_prod;

    while (pnt_curr_prod != (pnt_product_t) NULL)
    {
        if ((status = checkMediaType (msgDesc, pnt_curr_prod->mediacode,
            mediaType)) < IMS_OK)
        {
            /* Update item status to error (13). */
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Item '%d' of order '%d' has a media type mismatch.",
                pnt_curr_prod->item_id, pnt_curr_prod->order_id);
            errorFlag = IMS_TRUE;
        }

        pnt_last_dig_prod = pnt_curr_prod;
        pnt_curr_prod = pnt_curr_prod->pnt_next;
    }

    /*
    ** Check the errorFlag.
    */
    if (errorFlag == IMS_TRUE)
    {
        goto ERROR;
    }

    /*
    ** Check to see if we have a product list.
    */
    if (pnt_first_dig_prod == (pnt_product_t) NULL)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "The product list is empty.");
        goto ERROR;
    }

    /*
    ** Check to see if we are trying to put non-CEOS products
    ** onto a CEOS formatted tape.
    */
    if ((mediaFormat == IMS_CEOS) && (mediaType != IMS_DISK) &&
        (*ceosFlag == IMS_FALSE))
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "A CEOS formatted tape may not contain non-CEOS products.");
        goto ERROR;
    }

    /*
    ** If all of the products in the list are in the CEOS
    ** format then we want to create VDF and NDF files.
    */
    if (*ceosFlag == IMS_TRUE)
    {
        /*
        ** Check to see if the products will fit then
        ** create the VDF file.
        */
        if ((status = MediaFit (orderId, pnt_first_dig_prod,
            pnt_last_dig_prod, debug_print, pnt_vdfReq, msgDesc,
            no_size_check)) < IMS_OK)
        {
            goto ERROR;
        }

        /*
        ** Determine VDF and NDF file specifications.
        */
        (void) strcpy (vdfSpec, pnt_first_dig_prod->vdf_name);
        (void) strcpy (ndfSpec, pnt_first_dig_prod->vdf_name);
        (void) strcpy (qcSpec, vdfSpec);
        strLoc = ims_strIndex (ndfSpec, ".VDF");
        ndfSpec[strLoc+1] = 'N';

        /*
        ** If we are not doing a CEOS tape then create the NDF file
        ** in the staging area.
        */
        if ((mediaFormat != IMS_CEOS) || (mediaType == IMS_DISK))
        {
            if ((status = create_null_vdf (vdfSpec, ndfSpec,
                debug_print, msgDesc)) < IMS_OK)
            {
                (void) ims_msg (msgDesc, status,
                    "Could not create Null VDF file '%s'.",
                    ndfSpec);
                goto ERROR;
            }
        }
    }
    else
    {
        /*
        ** Make the VDF and NDF specifications zero length strings
        ** so that we won't include them in the TAR or FTP lists
        ** and also so we won't try to remove them later.
        */
        vdfSpec[0] = '\0';
        ndfSpec[0] = '\0';
    }

    /*
    ** Distribute the products to their selected media type.
    */
    if (mediaType != IMS_DISK)
    {
        if (mediaFormat != IMS_TAR)
        {
            /* Create CEOS tape. */
            if ((status = ims_formTapeCEOS (vdfSpec,
                deviceInfo->path, orderId, itemCount, itemArray,
                debug_print, dump_print, pnt_first_dig_prod->mediacode,
                pnt_vdfReq, msgDesc )) < IMS_OK)
            {
                goto ERROR;
            }
        }
        else
        {
            /* Create TAR tape. */
            if ((status = ims_formTapeTAR (msgDesc,
                vdfSpec, ndfSpec,  deviceInfo->path,
                orderId, itemCount, itemArray, pnt_vdfReq)) < IMS_OK)
            {
                (void) ims_msg(msgDesc, status,
                    "Could not create TAR archive on '%s'.",
                    deviceInfo->name);
                goto ERROR;
            }

            if ((reportType != NO_REPORT_TYPE) && (*ceosFlag == IMS_TRUE))
            {
                /*
                ** Get TAR stage area.
                */
                pnt_vdfReq->stage_type = QC_TAR;

                if ((status = ims_vdfCat (pnt_vdfReq,
                    VDF_GET_STAGE_AREA)) < IMS_OK)
                {
                    (void) ims_msg (msgDesc, status,
                        "Could not get staging area.");
                    goto ERROR;
                }

                /*
                ** Check the path retruned.
                */
                if ((pnt_vdfReq->path_name == (char *) NULL) ||
                    (strlen (pnt_vdfReq->path_name) == 0))
                {
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "The staging area value was null.");
                    goto ERROR;
                }

                /*
                ** Build stage area path
                */
                (void) sprintf (stageSpec, "%s/%ld%03d", pnt_vdfReq->path_name,
                    orderId, itemArray[0]);

                /*
                ** If the path exists, then remove it so we
                ** can rebuild the directory and extract the TAR
                ** files there.
                */
                while (mkdir (stageSpec, 0770) == -1)
                {
                    /* Check if dir already exists. */
                    if (errno == EEXIST)
                    {
                        /* Remove directory and try again. */
                        if ((status =
                            ims_removeStageDir (msgDesc, stageSpec)) < IMS_OK)
                        {
                            (void) ims_msg (msgDesc, status,
                                "Could not remove the stage path '%s'.",
                                stageSpec);
                            goto ERROR;
                        }
                        continue;
                    }

                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "Could not create the stage path '%s'. %s",
                        stageSpec, strerror (errno));
                    goto ERROR;
                }

                /*
                ** Un-TAR to stage area.
                */
                if ((status = ims_unTarfiles (msgDesc, deviceInfo->path,
                    stageSpec)) < IMS_OK)
                {
                    (void) ims_msg (msgDesc, status,
                        "Could not un-tar files to staging area from '%s'.",
                        deviceInfo->name);
                    goto ERROR;
                }

                /*
                ** Assign the stage area to the QC specification.
                */
                ims_concatFilePath (qcSpec,
                    stageSpec, ims_extractFileName (vdfSpec));
            }
        }
    }
    else /* Distribute products to electronic distribution sites. */
    {
        /*
        ** Get users local FTP area.
        */
        if ((status = ims_vdfCat (pnt_vdfReq,
            VDF_GET_FTP_AREA)) < IMS_OK)
        {
            (void) ims_msg (msgDesc, status,
                "Could not get local FTP area.");
            goto ERROR;
        }

        /*
        ** Check the path returned.
        */
        if ((pnt_vdfReq->path_name == (char *) NULL) ||
            (strlen (pnt_vdfReq->path_name) == 0))
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The local FTP area value was null.");
            goto ERROR;
        }

        /*
        ** Build stage area path
        */
        (void) sprintf (stageSpec, "%s/%ld%03d", pnt_vdfReq->path_name,
            orderId, itemArray[0]);

        /*
        ** If the path exists, then remove it so we
        ** can rebuild the directory and copy the
        ** files there.
        */
        while (mkdir (stageSpec, 0770) == -1)
        {
            /* Check if dir already exists. */
            if (errno == EEXIST)
            {
                /* Remove directory and try again. */
                if ((status =
                    ims_removeStageDir (msgDesc, stageSpec)) < IMS_OK)
                {
                    (void) ims_msg (msgDesc, status,
                        "Could not remove the stage path '%s'.",
                        stageSpec);
                    goto ERROR;
                }
                continue;
            }

            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not create the stage path '%s'. %s",
                stageSpec, strerror (errno));
            goto ERROR;
        }

        /*
        **Copy products to FTP site.
        */
        if (ims_sendRemoteFiles(msgDesc, vdfSpec, ndfSpec,
                stageSpec, orderId, itemList, pnt_vdfReq) < IMS_OK)
        {
            /* Failed, remove stage directory and exit. */
            (void) ims_msg (msgDesc, IMS_ERROR,
                "FTP Distribution failed.");

            if ((status =
                ims_removeStageDir (msgDesc, stageSpec)) < IMS_OK)
            {
                (void) ims_msg (msgDesc, status,
                    "Could not remove the stage path '%s'.",
                    stageSpec);
            }
            goto ERROR;
        }

        /*
        ** Change the mode of the directory.
        */
        if ((status = chmod (stageSpec, 0775)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not change the mode for the stage path '%s'. %s",
            stageSpec, strerror (errno));
            goto ERROR;
        }

        /*
        ** Assign the stage area to the QC specification.
        */
        ims_concatFilePath (qcSpec,
            stageSpec, ims_extractFileName (vdfSpec));
    }

    /*
    ** Remove the VDF and NDF files.
    */
    removeStageFiles (vdfSpec, ndfSpec);

    return (IMS_OK);

ERROR:
    /*
    ** Remove the VDF and NDF files.
    */
    removeStageFiles (vdfSpec, ndfSpec);

    if (status < IMS_OK)
    {
        return (status);
    }
    else
    {
        return (IMS_ERROR);
    }
}

/******************************************************************************
**
** checkMediaType ()
**
** Check the media type and convert it to its integer form.
**
******************************************************************************/

static int checkMediaType (
    IMS_MSG_STRUCT *msgDesc,
    char *mediaString,
    DBSMALLINT mediaType)
{
    DBSMALLINT mediaConvType;

    if (strcmp (mediaString, "4MML") == 0)
    {
        mediaConvType = IMS_4_MM;
    }
    else if (strcmp (mediaString, "4MMH") == 0)
    {
        mediaConvType = IMS_4_MM_HD;
    }
    else if (strcmp (mediaString, "8MML") == 0)
    {
        mediaConvType = IMS_8_MM;
    }
    else if (strcmp (mediaString, "8MMH") == 0)
    {
        mediaConvType = IMS_8_MM_HD;
    }
    else if (strcmp (mediaString, "CCTL") == 0)
    {
        mediaConvType = IMS_9_TRACK;
    }
    else if (strcmp (mediaString, "CCTH") == 0)
    {
        mediaConvType = IMS_9_TRACK_HD;
    }
    else if (strcmp (mediaString, "DISK") == 0)
    {
        mediaConvType = IMS_DISK;
    }
    else
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Media type '%s' is invalid.", mediaString);
        return (IMS_ERROR);
    }

    /*
    ** Check for a match with the given type.
    */
    if (mediaType != mediaConvType)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Media type '%s' does not match the given media type '%s'.",
            mediaString, ims_mediaDesc (mediaType));
        return (IMS_ERROR);
    }

    return (IMS_OK);
}

/******************************************************************************
**
** removeStageFiles ()
**
** Remove the VDF and NDF files from the staging area.
**
******************************************************************************/

static void removeStageFiles (
    char *vdfSpec,
    char *ndfSpec)
{
    if ((int) strlen (vdfSpec) > 0)
    {
        (void) remove (vdfSpec);
    }

    if ((int) strlen (ndfSpec) > 0)
    {
        (void) remove (ndfSpec);
    }

    return;
}
