/*==============================================================================
Filename:	faifdefs.h
Description:	
	Header file containing global #defines, typedefs.  This is the main
header include file which defines common types and constant definitions.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
	%W%
==============================================================================*/

#ifndef _FAIFDEFS_
#define _FAIFDEFS_

#include <syslog.h>

/* FAIF's log facility
*/
#define SYSLOG_FAIF (LOG_LOCAL6)

/* Name of environment variable for the FAIF root path
*/
#define FAIF_ROOTPATH_EV    "FAIF_ROOTPATH"
#define FAIF_ROOTPATH_DEF   "/local/faif"

#define FAIF_BINPATH_EV     "FAIF_BINPATH"
#define FAIF_BINPATH_DEF    "/asf/bin"

#define OK        0
#define ACCEPT    0
#define REJECT    1
#define ERROR    -1
#define SENTINEL -1

#define ESA      0
#define NASDA    1
#define CSA      2
#define WALPS    3
#define ADEOS    4

#define ESA_STR     "ESA"
#define NASDA_STR   "NASDA"
#define CSA_STR     "CSA"
#define WALPS_STR   "WALPS"
#define ADEOS_STR   "ADEOS"

#define FTP  0
#define NFS  1
#define FTAM 2

#define MAX_FILENAME_LEN 132
#define MAX_DIRNAME_LEN  132 
#define MAXLINE          256
#define MAX_SAT_ID_LEN     8
#define TIME_STRING_LEN   21

#define MAX_SYSLOG_MSGLEN 256

typedef unsigned char BYTE ;
typedef unsigned char LOGICAL ;

#ifndef TRUE
#define TRUE  (LOGICAL)1
#endif

#ifndef FALSE
#define FALSE (LOGICAL)0
#endif

#define MAX_NAME_SIZE 256

/* Used mainly for tables of config items
*/
typedef struct name_table_entry
{
   int  name_id ;
   char name_identifier[MAX_NAME_SIZE] ;
   char default_value[MAXLINE+1] ;

} Names_Table_Entry ;


#define MAX_FILE_ID_SIZE 132

/* Used for tables of file ids and file types
*/
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


/* File type and metadata information
*/
typedef struct _FA_filetype
{
   char sp_filetype[MAXLINE];
   char gen_filetype[MAXLINE];
   char agency[MAXLINE];
   char format[MAXLINE];
   char sat[MAXLINE];
   char dest[MAXLINE];
   char flag[MAXLINE];

} FA_filetype;	

/* For list of files and associated PMF files generated
*/
typedef struct PMF_file_list
{
   char file_name[MAX_FILENAME_LEN] ;
   char PMF_file_name[MAX_FILENAME_LEN] ;
   char dataset_suffix[MAXLINE] ;
   int orig_not_tran ;
 
} PMF_FILE_LIST ;

#endif /* _FAIFDEFS_ */

/* End of File */
