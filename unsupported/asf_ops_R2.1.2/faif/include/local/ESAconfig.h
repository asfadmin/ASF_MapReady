/*==============================================================================
Filename:	ESAconfig.h

Description:	
	Contains tables of config variables and default config settings 
for ESAdirmon program.  Includes names of config file, log file, source 
and destination hosts, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ESACONFIG_
#define _ESACONFIG_

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
-- 	XXX_TRANSPROTO        - File transfer mechanism/FTP script
-- where XXX is ESA
*/

/* ESA default configuration
*/
#define ESA_CONFIGFILE_DEF "ESA/config/ESAdirmon.config"
#define ESA_LOGFILE_DEF    "ESA/log/ESAdirmon.log"

#define ESA_DESTHOST_DEF   "" /* Ignored */
#define ESA_TRANSPROTO_DEF "" /* Ignored */
#define ESA_RECEPTDIR_DEF  "" /* Ignored */
#define ESA_DESTDIR_DEF    "" /* Ignored */
#define ESA_TRANSLDIR_DEF  "" /* Ignored */
#define ESA_SRCDIR_DEF     "" /* Ignored */
#define ESA_SRCHOST_DEF    "" /* Ignored */


/* Table of ESA Config Environment Variable Names
*/
Names_Table_Entry ESA_Config_Names_Table[] =
{
   { CONFIG_FILE,       "ESA_CONFIGFILE", ESA_CONFIGFILE_DEF },
   { LOG_FILE,          "ESA_LOGFILE",    ESA_LOGFILE_DEF    },
   { SRC_DIR,           "ESA_SRCDIR",     ESA_SRCDIR_DEF     },
   { SRC_HOST,          "ESA_SRCHOST",    ESA_SRCHOST_DEF    },
   { DEST_DIR,          "ESA_DESTDIR",    ESA_DESTDIR_DEF    },
   { DEST_HOST,         "ESA_DESTHOST",   ESA_DESTHOST_DEF   },
   { RECEPT_DIR,        "ESA_RECEPTDIR",  ESA_RECEPTDIR_DEF  },
   { TRANSFER_PROTOCOL, "ESA_TRANSPROTO", ESA_TRANSPROTO_DEF },
   { ROOT_PATH,         "FAIF_ROOTPATH",  FAIF_ROOTPATH_DEF  },
   { TRANS_DIR,         "ESA_TRANSLDIR",  ESA_TRANSLDIR_DEF  },
   { SENTINEL,           NULL,            NULL               }
} ;


#endif  /* _ESACONFIG_ */

/* End of file */
