/************************************************************************* 
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
** 
** File :  ims_v0Package.h 
**
** Function: This is the header file for the definitions related to IMS V0
**           server dynamic packaging handling
**
** Creator:  Julie Wang
**
** Date:     Sep 9, 1996 
**
*************************************************************************/

#ifndef _IMS_V0PACKAGE_H
#define _IMS_V0PACKAGE_H

static char *sccsv0Package = "@(#)ims_v0Package.h	1.1  12/19/96";
 
#include <ims_hash.h>

/*
** global variables. 
*/
IMS_HASH_STRUCT *v0_package_HashPtr;
AGGREGATE pkg_root_tree;

typedef struct pkg_media_format 
{
	char   v0_media_fmt[IMS_COL30_LEN+1];
	float  cost;
	struct pkg_media_format *next_p;
} PKG_MEDIA_FORMAT;

typedef struct pkg_media_type 
{
	char              v0_media_type[IMS_COL30_LEN+1];
	PKG_MEDIA_FORMAT  *media_fmt_list;
	int               mf_count;
	struct pkg_media_type *next_p;
} PKG_MEDIA_TYPE;

typedef struct pkg_proc_option 
{
	char              v0_process_type[IMS_COL30_LEN+1];
	char              pkg_size[IMS_COL30_LEN+1];
	PKG_MEDIA_TYPE    *media_type_list;
	int               mt_count;
	struct pkg_proc_option *next_p;
} PKG_PROC_OPTION;

typedef struct pkg_dataset 
{
	char    dataset_id[IMS_COL80_LEN+1];
	char    platform[IMS_COL30_LEN+1];
	char    sensor[IMS_COL30_LEN+1];
	int     proc_count;    
	PKG_PROC_OPTION *proc_options_list;
	struct pkg_dataset *next_p;
} PKG_DATASET;

typedef struct v0_pkg_struct
{
	int   ds_count;
	PKG_DATASET *dataset_list;
} V0_PKG;

/* ims_v0Package */
int v0_package__build_packageHash (IMS_MSG_STRUCT *, char *, char *, 
     V0_CAT_STRUCT *, int);

#endif /* !_IMS_V0PACKAGE_H */
