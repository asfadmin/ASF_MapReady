/******************************************************************************
** 
** File:        ims_const.h
**
** Modified:    11/28/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#ifndef _IMS_CONST_H
#define _IMS_CONST_H

static char *sccsConst = "@(#)ims_const.h	5.3  05/07/97";

#define IMS_MAX(x,y) ((x) > (y) ? (x) : (y))

#define IMS_MAXTRIES 3

#define IMS_ON    1
#define IMS_OFF   0
#define IMS_TRUE  1
#define IMS_FALSE 0

#define IMS_MAX_TINT 255
#define IMS_MAX_SINT 32767
#define IMS_MAX_INT  2147483647

#define IMS_NAME_LEN           30  /* Should be equal to DBMAXNAME. */
#define IMS_HOST_LEN           30
#define IMS_PROGRAM_LEN        30
#define IMS_DATETIME_LEN       21
#define IMS_DBMS_DATETIME_LEN  26
#define IMS_MSG_TIME_LEN       26
#define IMS_PATH_LEN          255

#define IMS_PAGE 512
#define IMS_COL1_LEN 1
#define IMS_COL2_LEN 2
#define IMS_COL3_LEN 3
#define IMS_COL4_LEN 4
#define IMS_COL5_LEN 5
#define IMS_COL10_LEN 10
#define IMS_COL11_LEN 11
#define IMS_COL15_LEN 15
#define IMS_COL16_LEN 16
#define IMS_COL20_LEN 20
#define IMS_COL30_LEN 30
#define IMS_COL40_LEN 40
#define IMS_COL45_LEN 45
#define IMS_COL50_LEN 50
#define IMS_COL60_LEN 60
#define IMS_COL64_LEN 64
#define IMS_COL80_LEN 80
#define IMS_COL128_LEN 128
#define IMS_COL255_LEN 255
#define IMS_COL512_LEN 512
#define IMS_COL1024_LEN 1024
#define IMS_KEYWORD_LEN	20
#define IMS_KEYWORD_LIST_LEN 1024
#define IMS_VAL_LEN 45
#define IMS_LENGTH_FIELD_LEN 8
#define IMS_VALUE_FIELD_LEN 5096	
#define IMS_SEARCH_BUF_LEN 1024
#define IMS_COMMENT_LEN 512
#define IMS_BADSIG (void(*)()) -1
#define IMS_HEADER_LEN 20
#define IMS_TYPE_LEN 12
#define IMS_MAX_BUFFER_SIZE 255
#define IMS_MAX_BUFFER_COUNT 39
#define IMS_INPUT_BUF_LEN 255

#endif	/* !_IMS_CONST_H */
