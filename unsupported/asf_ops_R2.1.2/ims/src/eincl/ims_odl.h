/******************************************************************************
**
** File:        ims_odl.h
**
** Function:    Include file for IMS ODL functions.
**
** Author:      Dan Crichton
**
** Date:        9/27/95
**
******************************************************************************/

#ifndef _IMS_ODL_H
#define _IMS_ODL_H

static char *sccsOdl = "@(#)ims_odl.h	5.3  12/19/96";

/*
** Definitions
*/
#define     IMS_OBJECT              1
#define     IMS_KEYWORD             2
#define     IMS_ODL_GROUP           3

/*
** Tree structure definition.
*/
typedef struct ims_odl_tree
{
    char *node_name;
    VALUE_DATA value;
    int type;
    char *data; /* optional data pointer */
    struct ims_odl_tree *children;
    struct ims_odl_tree *next;
} IMS_ODL_TREE;

/*
** Keyword Array structure definition.
*/
typedef struct ims_keyword_array
{
    char *label;
    char *value;
    int type; /* See odldef.h for data types. */
} IMS_KEYWORD_ARRAY;

/*
** Keyword list structure definition.
*/
typedef struct ims_keywordList
{
    char keyword[IMS_COL30_LEN+1];
    int item_type;
    int data_type;
    int value_integer;
    double value_real;
    char value_string[IMS_COL255_LEN+1];
    struct ims_keywordList *next;
} IMS_KEYWORD_LIST;

/*
** Function prototypes for the ims_odl.c module.
*/
int ims_addODLObject(IMS_MSG_STRUCT *, IMS_ODL_TREE *, IMS_ODL_TREE **,
    char *, int, int );
int ims_addODLKeyword(IMS_MSG_STRUCT *, IMS_ODL_TREE *, char *, int, char *);
int ims_buildPMF(IMS_MSG_STRUCT *, IMS_ODL_TREE *, char *, FILE *);
int ims_create_cmn_hdr(IMS_MSG_STRUCT *, IMS_ODL_TREE **, char *, int);
int ims_create_catalog(IMS_MSG_STRUCT *, IMS_ODL_TREE *, IMS_KEYWORD_ARRAY [],
    int);
int ims_buildAggregate(IMS_MSG_STRUCT *, IMS_ODL_TREE *, AGGREGATE *);
int ims_parseODLFile (IMS_MSG_STRUCT *, char *, char *, IMS_KEYWORD_LIST **);
int ims_parseODLBuffer (IMS_MSG_STRUCT *, char *, char *, IMS_KEYWORD_LIST **);
int ims_findListItem (IMS_MSG_STRUCT *, IMS_KEYWORD_LIST *, char *, int,
    int, IMS_KEYWORD_LIST **);
void ims_freeKeywordList (IMS_KEYWORD_LIST *);
int ims_ODLFileToTree (IMS_MSG_STRUCT *, char *, IMS_ODL_TREE **);
int ims_attachODLData(IMS_ODL_TREE *, char *);
int ims_delete_tree (IMS_MSG_STRUCT *, IMS_ODL_TREE *);

#endif  /* !_IMS_ODL_H */
