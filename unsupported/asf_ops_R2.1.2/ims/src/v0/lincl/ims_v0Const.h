/************************************************************************* 
** 
** File :  ims_v0Const.h 
**
** Function: Defines constants used in IMS V0 server application.
**
** Creator: Julie Wang
**
** Date:    May 1, 1996
**
*************************************************************************/

#ifndef _IMS_V0CONST_H
#define _IMS_V0CONST_H

static char *sccsv0Const = "@(#)ims_v0Const.h	1.5  11/14/97";
 
#define V0_MAX_CHUNK_GRANULES  	30
#define V0_MAX_COMMENT_LENGTH  	60
#define V0_MAX_WHERE_LEN   	5120 

#define V0_ORDER_ERROR		"99"

/*
** maximum length of text fields in incoming ODL thru ims_order interface
*/
#define MAX_AUTH          20
#define MAX_REQ_ID        30
#define MAX_NAME          20
#define MAX_MID_NAME       1
#define MAX_ADDR          32
#define MAX_CITY          30
#define MAX_STATE         20
#define MAX_ZIP           10
#define MAX_COUNTRY       20
#define MAX_PHONE         22
#define MAX_EMAIL        128
#define MAX_TITLE          5
#define MAX_ORG           31
#define MAX_CAT            7
#define MAX_AFFL_TYPE     10
#define MAX_DS_ID         80
#define MAX_GNUL_ID       30
#define MAX_PROC_OPT      30
#define MAX_MEDIA_TYPE    30
#define MAX_MEDIA_FMT     30
#define MAX_BILL_ID       15
#define MAX_PLAT          30
#define MAX_SENSOR        30
#define MAX_MODE          30
#define MAX_QL             1
#define MAX_FREQ          30
#define MAX_DIRECTION      1
#define MAX_S_SHAPE        1
#define MAX_S_NAME        30
#define MAX_PRIORITY       7
#define MAX_ACT_P          1
#define MAX_S_FIELD       21
#define MAX_E_FIELD       21
#define MAX_DAR_COMT     255
#define MAX_MSG_ID	  30

#endif /* !_IMS_V0CONST_H */
