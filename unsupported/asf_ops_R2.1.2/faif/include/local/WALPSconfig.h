/*==============================================================================
Filename:	WALPSconfig.h

Description:	
	Contains default config settings for WALPSdirmon programs.
Includes names of config file, log file, source and destination hosts, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _WALPSCONFIG_
#define _WALPSCONFIG_

/*
#include "faifdefs.h"
*/
#include "configrec.h"

/* Config/Environment Variable Names
--
-- A subset of names of environment variables that can be set to
-- specify the configuration of a directory monitor are
-- listed below.  
--
--      FAIF_ROOTPATH         - Root pathname of reception directory,
--                              config file, log file.
-- 	XXX_CONFIGFILE        - Name of config file
-- 	XXX_LOGFILE           - Directory monitor log file
-- 	XXX_DESTHOST          - Files destination machine
-- 	XXX_TRANSPROTO        - File transfer mechanism
-- where XXX is WALPS
*/

/* WALLOPS default configuration
*/
#define WALPS_CONFIGFILE_DEF "WALPS/config/WALPSdirmon.config"
#define WALPS_LOGFILE_DEF    "WALPS/log/WALPSdirmon.log"

#define WALPS_DESTHOST_DEF   "" /* Ignored */
#define WALPS_TRANSPROTO_DEF "" /* Ignored */
#define WALPS_RECEPTDIR_DEF  "" /* Ignored */
#define WALPS_DESTDIR_DEF    "" /* Ignored */
#define WALPS_TRANSLDIR_DEF  "" /* Ignored */
#define WALPS_SRCDIR_DEF     "" /* Ignored */
#define WALPS_SRCHOST_DEF    "" /* Ignored */


Names_Table_Entry WALPS_Config_Names_Table[] =
{
   { CONFIG_FILE,       "WALPS_CONFIGFILE", WALPS_CONFIGFILE_DEF },
   { LOG_FILE,          "WALPS_LOGFILE",    WALPS_LOGFILE_DEF    },
   { SRC_DIR,           "WALPS_SRCDIR",     WALPS_SRCDIR_DEF     },
   { SRC_HOST,          "WALPS_SRCHOST",    WALPS_SRCHOST_DEF    },
   { DEST_DIR,          "WALPS_DESTDIR",    WALPS_DESTDIR_DEF    },
   { DEST_HOST,         "WALPS_DESTHOST",   WALPS_DESTHOST_DEF   },
   { RECEPT_DIR,        "WALPS_RECEPTDIR",  WALPS_RECEPTDIR_DEF  },
   { TRANSFER_PROTOCOL, "WALPS_TRANSPROTO", WALPS_TRANSPROTO_DEF },
   { ROOT_PATH,         "FAIF_ROOTPATH",    FAIF_ROOTPATH_DEF    },
   { TRANS_DIR,         "WALPS_TRANSLDIR",  WALPS_TRANSLDIR_DEF  },
   { SENTINEL,           NULL,              NULL                 }
} ;

#endif /* _WALPSCONFIG_ */

/* End of File */
