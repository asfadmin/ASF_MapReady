/*==============================================================================
Filename:	configrec.h
Description:	Config Type definitions header file
Creator:	Norbert Piega	
Notes:		

SCCS Info:
   %W%
==============================================================================*/
#ifndef _CONFIGREC_
#define _CONFIGREC_

#ifndef ERROR
#include "faifdefs.h"
#endif

#define MAX_CONFIG_FILENAME_LEN 80

#define CONFIG_FILE       0
#define LOG_FILE          1
#define SRC_DIR           2
#define SRC_HOST          3
#define DEST_DIR          4
#define DEST_HOST         5
#define RECEPT_DIR        6
#define TRANSFER_PROTOCOL 7
#define ROOT_PATH         8
#define TRANS_DIR         9


/* Config Record Structure
*/
typedef struct config_rec
{
   char *FA_srcdir ;       /* Directory in external host for incoming files */
   char *FA_srchost ;      /* Flight Agency source host */
   char *FA_destspec ;     /* Destination within ASF */
   char *FA_desthost ;     /* Destination Host within ASF */
   char *FA_receptdir ;    /* Directory in FAIF where received files are stored */
   char *FA_transdir ;     /* Directory in FAIF where translated files are stored */
   char *FA_configfile ;   /* Config filename */
   char *FA_logfile ;      /* Log file (log of routed files) */
   char *FA_transfercmd ;  /* Transfer script used for sending to ASF destination */
   char *FA_rootpath ;     /* Root path where FAIF dirs are built from */
   int   FA_filetype ;     /* Integer Id for a FA file type */

} Config_Record ;

#endif /* _CONFIGREC_ */

/* End of File */
