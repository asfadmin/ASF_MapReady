/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _DB_SCAN_ORDER_H
#define _DB_SCAN_ORDER_H

#pragma ident "@(#)db_scan_order.h	1.2  12/16/96"

#define NUM_SCAN_ORDER_COLS     	39

#define SCAN_ORDER_JOB_ID  		0
#define SCAN_ORDER_ORDER_ID    		1
#define SCAN_ORDER_ITEM_ID    		2
#define SCAN_ORDER_QUICKLOOK_FLAG	3
#define SCAN_ORDER_ORDER_TYPE  		4
#define SCAN_ORDER_PLATFORM  		5
#define SCAN_ORDER_SENSOR  		6
#define SCAN_ORDER_REV     		7
#define SCAN_ORDER_MODE    		8
#define SCAN_ORDER_SEQUENCE  		9
#define SCAN_ORDER_ACTIVITY_ID 		10
#define SCAN_ORDER_FRAME_MODE  		11
#define SCAN_ORDER_SITE_NAME  		12

#define SCAN_ORDER_MEDIA_TYPE  		13
#define SCAN_ORDER_START_ADDRESS  	14
#define SCAN_ORDER_END_ADDRESS  	15
#define SCAN_ORDER_START_TIME  		16
#define SCAN_ORDER_END_TIME  		17
#define SCAN_ORDER_RECORDER_ID  	18
#define SCAN_ORDER_STATION_ID  		19

#define SCAN_ORDER_GHA_TIME  		20
#define SCAN_ORDER_GHA_ANGLE  		21
#define SCAN_ORDER_SV_TYPE  		22
#define SCAN_ORDER_SV_COORD_SYS  	23
#define SCAN_ORDER_SV_REV  		24
#define SCAN_ORDER_SV_TIME  		25
#define SCAN_ORDER_SV_X_POS  		26
#define SCAN_ORDER_SV_Y_POS  		27
#define SCAN_ORDER_SV_Z_POS  		28
#define SCAN_ORDER_SV_X_VEL  		29
#define SCAN_ORDER_SV_Y_VEL  		30
#define SCAN_ORDER_SV_Z_VEL  		31
#define SCAN_ORDER_TCE_REV  		32
#define SCAN_ORDER_TCE_TIME  		33
#define SCAN_ORDER_TCE_SAT_TIME  	34
#define SCAN_ORDER_TCE_CLOCK_CYCLE  	35
#define SCAN_ORDER_MEDIA_LOCATION	36
#define SCAN_ORDER_MEDIA_ID		37
#define SCAN_ORDER_DATA_DIRECTION	38

#endif /* _DB_SCAN_ORDER_H */
