static char *sccs = "@(#)ims_formTape.c	5.2  03/07/97";
/******************************************************************************
**
** File:        ims_formTape.c
**
** Function:    This function generates a CEOS formatted tape.
**
** Author:      D. Pass
**
** Date:        5/17/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <ctype.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/ioctl.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_util.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <defs.h>

/*
** The following non-POSIX definitions are required by the header
** file that follows.
*/
typedef unsigned char u_char;
typedef unsigned short u_short;

#include    <sys/mtio.h>

/*
** External Function Prototypes.
** (These should be in a header file.)
*/
int process_vdf (char *, int, pnt_vdf_file_t, short, short, IMS_MSG_STRUCT *);
int process_ldr (char *, int, short, short, IMS_MSG_STRUCT *);
int process_dat (char *, int, short, IMS_MSG_STRUCT *);
int make_null_vdf (pnt_vdf_file_t, char *, short, int, IMS_MSG_STRUCT *);
void free_vdf (pnt_vdf_file_t);

/******************************************************************************
**
** ims_formTapeCEOS ()
**
** This function reads in the VDF file and writes the appropriate
** CEOS files in CEOS format to tape.
**
******************************************************************************/

int ims_formTapeCEOS (
    char *vdfSpec,
    char *targetPath,
    DBINT order_id,
    int itemCount,
    DBSMALLINT *items,
    int debug,
    int dump,
    char *media_type,
    pnt_vdf_cat_request_t pnt_vdfReq,
    IMS_MSG_STRUCT *msgDesc )
{
    pnt_vdf_file_t pnt_vdf;         /* VDF Info Structure */
    pnt_granule_data_t pnt_gdata;   /* Granule Info Structure */
    pnt_product_t pnt_prod;         /* Product Info Structure */
    struct mtop mt_command;         /* ioctl command structure  */
    struct mtget mt_status;         /* ioctl results structure  */
    char fileSpec[IMS_PATH_LEN+1];
    DBSMALLINT vdfItem;
    long strLoc;
    int i;
    int status;
    int fd;
    int maxlenTemp;
    int trailerFlag;
    int tapeFlag;


    (void) ims_msg( msgDesc, IMS_INFO,
        "Started CEOS formatted distribution for order %d, path '%s'.\n",
        order_id, targetPath);
    if (debug)
    {
        (void) printf (
            "\nStarted CEOS formatted distribution for order %d, "
            "path '%s'.\n",order_id, targetPath);
    }

    /*
    ** Determine whether the target is a tape or a file.
    */
    if ((strLoc = ims_strIndex (targetPath, "/dev/")) == -1)
    {
        tapeFlag = IMS_FALSE;
    }
    else
    {
        tapeFlag = IMS_TRUE;
    }

    /*
    ** Open the target.
    */
    if ((fd = open (targetPath, O_WRONLY|O_CREAT|O_TRUNC)) == -1)
    {
        (void) ims_msg (msgDesc, IMS_ERROR,
            "Could not open target '%s'. %s",
            targetPath, strerror (errno));
        return (IMS_ERROR);
    }

    /*
    ** Change the mode of the target.
    */
    if (tapeFlag == IMS_FALSE)
    {
        if ((status = chmod (targetPath, 0755)) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not change the mode for target '%s'. %s",
                targetPath, strerror (errno));
            (void) close (fd);
            return (IMS_ERROR);
        }
    }

    /*
    ** Allocate space for the pnt_vdf_file_t structure.
    */
    if ((pnt_vdf = (pnt_vdf_file_t) malloc
        ((size_t) sizeof (vdf_file_t))) ==
        (pnt_vdf_file_t) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for the pnt_vdf_file_t structure.");
        (void) close (fd);
        return (IMS_FATAL);
    }

    /*
    ** Rewind the target tape if necessary.
    */
    if (tapeFlag == IMS_TRUE)
    {
        mt_command.mt_op = MTREW;
        mt_command.mt_count = 1;

        if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not send rewind command to '%s'. %s",
                targetPath, strerror (errno));
            status = IMS_ERROR;
            goto ERROR;
        }

        /*
        ** Get the status info.
        */
        mt_status.mt_erreg = 0;
        if (ioctl (fd, MTIOCGET, &mt_status) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not get status information from '%s'. %s",
                targetPath, strerror (errno));
            status = IMS_ERROR;
            goto ERROR;
        }
        else
        {
            if (mt_status.mt_erreg < 0)
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not rewind target tape '%s'. %s",
                    targetPath, strerror (errno));
                status = IMS_ERROR;
                goto ERROR;
            }
        }
    }

    if (debug)
    {
        (void) printf ("\nProcessing VDF file '%s'.\n", vdfSpec);
    }

    /*
    ** Read the VDF file and write it to the target.
    */
    if ((status = process_vdf (vdfSpec, fd, pnt_vdf, debug,
        dump, msgDesc)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Write an EOF marker to the target tape.
    */
    if (tapeFlag == IMS_TRUE)
    {
        mt_command.mt_op = MTWEOF;
        mt_command.mt_count = 1;

        if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not send EOF command to '%s'. %s",
                targetPath, strerror (errno));
            status = IMS_ERROR;
            goto ERROR;
        }
    }

    /*
    ** Allocate space for the pnt_product_t structure.
    */
    if ((pnt_prod = (pnt_product_t) malloc
        ((size_t) sizeof (product_t))) ==
        (pnt_product_t) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for the pnt_product_t structure.");
        status = IMS_FATAL;
        goto ERROR;
    }

    pnt_vdfReq->pnt_save_prod = pnt_prod;
    pnt_prod->order_id = order_id;
    pnt_gdata = pnt_vdf->pnt_gdata;

    /*
    ** Go through the item list and write the CEOS product
    ** files to the target for the current item.
    */
    for (i = 0; i < itemCount; i++)
    {
        /*
        ** Compare the item id with the VDF item id.
        */
        vdfItem = (DBSMALLINT) atoi (pnt_gdata->pnt_file_rec->orderline);
        if (vdfItem != ( items[i] % 100 ) )
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The current item '%d' does not match the VDF item '%d'.",
                items[i], vdfItem);
            status = IMS_ERROR;
            goto ERROR;
        }
        pnt_prod->item_id = items[i];

        /*
        ** Get the file name and format of the product for the current item.
        */
        if ((status = ims_vdfCat (pnt_vdfReq, VDF_GET_FILE_NAME)) < IMS_OK)
        {
            goto ERROR;
        }

        /*
        ** Compare the product name with the VDF product name.
        */
        if (strcmp (pnt_prod->name,
            pnt_gdata->pnt_file_rec->file_name) != 0)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "The current product name '%s' does not match the VDF file product name '%s'.",
                pnt_prod->name, pnt_gdata->pnt_file_rec->file_name);
            status = IMS_ERROR;
            goto ERROR;
        }

        /*
        ** Check the status of the granule.
        */
        if (pnt_prod->status != GRANULE_AVAILABLE)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Granule '%s', associated with item '%d' of order '%d', does not have a status of 'Available'.",
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
                "Granule '%s', associated with item '%d' of order '%d', uses internal versioning which is not supported.",
                pnt_prod->name, pnt_prod->item_id, pnt_prod->order_id);
            return (IMS_ERROR);
        }

        (void) ims_msg( msgDesc, IMS_INFO,
            "Processing item no. %d, item id %d, product '%s'.",
            i+1, items[i], pnt_prod->name);
        if(  debug ){
            (void) printf(
                "\nProcessing item no. %d, item id %d, product '%s'.\n",
                i+1, items[i], pnt_prod->name);
        }
        /*
        ** Get the path of the product files for the current item.
        ** This query sets the maxlen variable if a trailer exists.
        */
        maxlenTemp =  pnt_vdfReq->maxlen;
        pnt_vdfReq->maxlen = 0;
        if ((status  = ims_vdfCat (pnt_vdfReq, VDF_GET_PATH)) < IMS_OK)
        {
            goto ERROR;
        }

        /*
        ** Check for a trailer file.
        */
        if (pnt_vdfReq->maxlen == 0)
        {
            trailerFlag = IMS_TRUE;
        }
        else
        {
            trailerFlag = IMS_FALSE;
        }
        pnt_vdfReq->maxlen = maxlenTemp;

        /*
        ** Set-up the leader file specification.
        */
        ims_concatFilePath (fileSpec, pnt_vdfReq->path_name,
            pnt_gdata->pnt_file_rec->file_name);
        (void) strcat (fileSpec, ".L" );

        if (debug)
        {
            (void) printf ("Processing leader file '%s'.\n", fileSpec);
        }

        /*
        ** Process the leader file.
        */
        if ((status = process_ldr (fileSpec, fd, debug, dump,
            msgDesc)) < IMS_OK)
        {
            goto ERROR;
        }

        /*
        ** Write an EOF marker to the target tape.
        */
        if (tapeFlag == IMS_TRUE)
        {
            mt_command.mt_op = MTWEOF;
            mt_command.mt_count = 1;

            if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not send EOF command to '%s'. %s",
                    targetPath, strerror (errno));
                status = IMS_ERROR;
                goto ERROR;
            }
        }

        /*
        ** Set-up the data file specification.
        */
        ims_concatFilePath (fileSpec, pnt_vdfReq->path_name,
            pnt_gdata->pnt_file_rec->file_name);
        (void) strcat (fileSpec, ".D" );

        if (debug)
        {
            (void) printf ("Processing data file '%s'.\n", fileSpec);
        }

        /*
        ** Process the data file.
        */
        if ((status = process_dat (fileSpec, fd, debug,
            msgDesc)) < IMS_OK)
        {
            goto ERROR;
        }

        /*
        ** Write an EOF marker to the target tape.
        */
        if (tapeFlag == IMS_TRUE)
        {
            mt_command.mt_op = MTWEOF;
            mt_command.mt_count = 1;

            if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
            {
                (void) ims_msg (msgDesc, IMS_ERROR,
                    "Could not send EOF command to '%s'. %s",
                    targetPath, strerror (errno));
                status = IMS_ERROR;
                goto ERROR;
            }
        }

        /*
        ** See if we have a trailer file.
        */
        if (trailerFlag == IMS_TRUE)
        {
            /*
            ** Set-up trailer file specification.
            */
            ims_concatFilePath (fileSpec, pnt_vdfReq->path_name,
                pnt_gdata->pnt_file_rec->file_name);
            (void) strcat (fileSpec, ".T" );

            if (debug)
            {
                (void) printf ("Processing trailer file '%s'.\n", fileSpec);
            }

            /*
            ** Process the trailer file.
            */
            if ((status = process_ldr (fileSpec, fd, debug, dump,
                msgDesc)) < IMS_OK)
            {
                goto ERROR;
            }

            /*
            ** Write an EOF marker to the target tape.
            */
            if (tapeFlag == IMS_TRUE)
            {
                mt_command.mt_op = MTWEOF;
                mt_command.mt_count = 1;

                if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
                {
                    (void) ims_msg (msgDesc, IMS_ERROR,
                        "Could not send EOF command to '%s'. %s",
                        targetPath, strerror (errno));
                    status = IMS_ERROR;
                    goto ERROR;
                }
            }
        }
        if (debug)
        {
            (void) printf ("Finished processing item %d in list.\n",
                i );
        }
        pnt_gdata = pnt_gdata->pnt_next;
    }

    /*
    ** Set-up the NULL VDF file specification.
    */
    (void) strcpy (fileSpec, vdfSpec);
    strLoc = ims_strIndex (fileSpec, ".VDF");
    fileSpec[strLoc+1] = 'N';

    if (debug)
    {
        (void) printf ("Processing NDF file '%s'.\n", fileSpec);
    }

    /*
    ** Write the NULL VDF file to the target.
    */
    if ((status = make_null_vdf (pnt_vdf, fileSpec, debug, fd,
        msgDesc)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Write two EOF markers to the target tape.
    */
    if (tapeFlag == IMS_TRUE)
    {
        mt_command.mt_op = MTWEOF;
        mt_command.mt_count = 2;

        if (ioctl (fd, MTIOCTOP, &mt_command) == -1)
        {
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Could not send EOF commands to '%s'. %s",
                targetPath, strerror (errno));
            status = IMS_ERROR;
            goto ERROR;
        }
    }

    /*
    ** Close the target and free the allocated structures.
    */
    (void) close (fd);
    free (pnt_prod);
    free_vdf (pnt_vdf);

    (void) ims_msg (msgDesc, IMS_INFO,
        "Completed CEOS formatted distribution for '%s'.\n", targetPath);
    if (debug)
    {
        (void) printf ("\nCompleted CEOS formatted distribution for '%s'.\n",
            targetPath);
    }

    return (IMS_OK);

ERROR:
    /*
    ** Close the target and free the allocated structures.
    */
    (void) close (fd);
    free (pnt_prod);
    free_vdf (pnt_vdf);

    return (status);
}
