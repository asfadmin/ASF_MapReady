/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	defs.h
Description:	
	Header file containing global #defines, typedefs.  This is the main
header include file which defines common types and constant definitions.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

==============================================================================*/

#ifndef _DEFS_
#define _DEFS_

#pragma ident "@(#)defs.h	1.1  11/21/96"

#include <syslog.h>

/* PPS's log facility
*/
#define SYSLOG_PPS (LOG_LOCAL2)

/* Name of environment variable for the PPS root path
*/

#define OK        0
#define ACCEPT    0
#define REJECT    1
#define ERROR    -1
#define SENTINEL -1

#define MAX_FILENAME_LEN 132
#define MAX_DIRNAME_LEN  132 
#define MAXLINE          256
#define MAX_SAT_ID_LEN     8
#define TIME_STRING_LEN   21

#define MAX_SYSLOG_MSGLEN 5000

typedef unsigned char BYTE ;
typedef unsigned char LOGICAL ;

#ifndef TRUE
#define TRUE  (LOGICAL)1
#endif

#ifndef FALSE
#define FALSE (LOGICAL)0
#endif

#define MAX_NAME_SIZE 256

#define ElementNumber(arr) ((unsigned int)(sizeof(arr)/sizeof(arr[0])))

#define StructOffset(type_ptr,field) \
           ((unsigned int)&(((type_ptr)NULL)->field))

#define PPS_ENV_ASF	"ASF"
#define PPS_ENV_LOCAL	"LOCAL"
	 
typedef enum
{
        PPS_CRITICAL,
        PPS_ERROR,
        PPS_WARNING,
        PPS_INFO,
        PPS_DEBUG

} PPS_SYSLOG_LEVEL_E;

typedef enum
{
/* these are in seconds */
        E1_ORBIT_TIME= 6060,    /* 101 minutes */
        E2_ORBIT_TIME= 6060,
        J1_ORBIT_TIME= 5820,    /* 97 minutes */
        R1_ORBIT_TIME= 6060,
	DEFAULT_ORBIT_TIME = 6060,
	TCE_CYCLE_TIME= 129600,  /* 36 hours */
	GHA_CYCLE_TIME= 1814400 /* 3 weeks */
} PPS_CYCLE_TIME_E;

typedef struct name_table_entry
{
   int  name_id ;
   char name_identifier[MAX_NAME_SIZE] ;
   char default_value[MAXLINE+1] ;
} Names_Table_Entry ;


#define MAX_FILE_ID_SIZE 132

typedef struct file_id
{
   int  file_id_number ;
   char file_identifier[MAX_FILE_ID_SIZE] ;
} File_Identifier ;


/* struct for File Parser Function Table entry
*/
typedef struct function_table_entry
{
   int func_id ;
   char *func_name ;
   int (*func_entry)() ;
} Function_Table_Entry ;


/* struct for semaphore key table per type of owner process
*/
typedef struct semkey
{
   int type_id ;    /* id for owner of semaphore */
   int key ;        /* semaphore key */

} Semaphore_Key_Table_Entry ;

#endif /* _DEFS_ */

/* End of File */
