/*****************************************************************************
**
** File:    ims_odlBuffer.h
**
** Function: Include file for IMS ODL functions.
**
** Author: Dan Crichton
**
** Date:    9/27/95
**
**
*****************************************************************************/

#ifndef _IMS_ODLBUFFER_H
#define _IMS_ODLBUFFER_H

static char *sccsOdlBuffer = "@(#)ims_odlBuffer.h	5.1  16 Mar 1996";

/*
** Definitions.
*/
#define IMS_OBJECT  1
#define IMS_KEYWORD 2
#define MAX_ODL     100   /* no. of odl pairs per buffer */
#define MAX_ODL_BUF  2048   /* size of max odl buffer */


typedef struct ims_keyword_array
{
    char *label;
    char *value;
    int type; /* See odldef.h for data types. */
} IMS_KEYWORD_ARRAY;

typedef struct ims_keyword_odl
{
    char * keyword;
    char * value;
} IMS_KEYWORD_ODL;

/*
** buffer to hold odl words
*/
typedef struct  ims_keyword_list *pnt_ims_keyword_list_t;
typedef struct  ims_keyword_list
{
    IMS_KEYWORD_ODL ary[MAX_ODL];
    pnt_ims_keyword_list_t  next;
} IMS_KEYWORD_LIST;

#endif  /* !_IMS_ODLBUFFER_H */
