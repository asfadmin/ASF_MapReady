/*==============================================================================
Filename:	ADEOSconfig.h

Description:	
	Contains default config settings for ADEOSdirmon programs.
Includes names of config file, log file, source and destination hosts, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ADEOSCONFIG_
#define _ADEOSCONFIG_

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
-- where XXX is ADEOS
*/

/* WALLOPS default configuration
*/
#define ADEOS_CONFIGFILE_DEF "ADEOS/config/ADEOSdirmon.config"
#define ADEOS_LOGFILE_DEF    "ADEOS/log/ADEOSdirmon.log"
#define ADEOS_DESTHOST_DEF   "" /* Ignored */
#define ADEOS_TRANSPROTO_DEF "" /* Ignored */

#define ADEOS_RECEPTDIR_DEF  "" /* Ignored */
#define ADEOS_DESTDIR_DEF    "" /* Ignored */
#define ADEOS_TRANSLDIR_DEF  "" /* Ignored */
#define ADEOS_SRCDIR_DEF     "" /* Ignored */
#define ADEOS_SRCHOST_DEF    "" /* Ignored */


Names_Table_Entry ADEOS_Config_Names_Table[] =
{
   { CONFIG_FILE,       "ADEOS_CONFIGFILE", ADEOS_CONFIGFILE_DEF },
   { LOG_FILE,          "ADEOS_LOGFILE",    ADEOS_LOGFILE_DEF    },
   { SRC_DIR,           "ADEOS_SRCDIR",     ADEOS_SRCDIR_DEF     },
   { SRC_HOST,          "ADEOS_SRCHOST",    ADEOS_SRCHOST_DEF    },
   { DEST_DIR,          "ADEOS_DESTDIR",    ADEOS_DESTDIR_DEF    },
   { DEST_HOST,         "ADEOS_DESTHOST",   ADEOS_DESTHOST_DEF   },
   { RECEPT_DIR,        "ADEOS_RECEPTDIR",  ADEOS_RECEPTDIR_DEF  },
   { TRANSFER_PROTOCOL, "ADEOS_TRANSPROTO", ADEOS_TRANSPROTO_DEF },
   { ROOT_PATH,         "FAIF_ROOTPATH",    FAIF_ROOTPATH_DEF    },
   { TRANS_DIR,         "ADEOS_TRANSLDIR",  ADEOS_TRANSLDIR_DEF  },
   { SENTINEL,           NULL,              NULL                 }
} ;

#endif /* _ADEOSCONFIG_ */

/* End of File */
