/******************************************************************************
**
** File:        ims_tar.h
**
** Function:    Include file for tar services.
**
** Author:      Dan Crichton	
**
** Date:        8/21/95
**
******************************************************************************/

#ifndef _IMS_TAR_H
#define _IMS_TAR_H

static char *sccsTar = "@(#)ims_tar.h	5.3  07/25/96";

typedef struct ims_tar_list
{
	char *filename;
	char *path;
	MEDIA_ITEM_LIST *itemPtr;
	struct ims_tar_list *next;
} IMS_TAR_LIST;

/*
** Function prototypes for the ims_tar.c module.
*/
int ims_sendRemoteFiles(IMS_MSG_STRUCT *, char *, char *, char *, int,
	MEDIA_ITEM_LIST *, pnt_vdf_cat_request_t);
int ims_formTapeTAR (IMS_MSG_STRUCT *, char *, char *, char *, int, int,
	DBSMALLINT [], pnt_vdf_cat_request_t);
int ims_removeStageDir (IMS_MSG_STRUCT *, char *);
int ims_unTarfiles (IMS_MSG_STRUCT *, char *, char *);

#endif	/* !_IMS_TAR_H */

