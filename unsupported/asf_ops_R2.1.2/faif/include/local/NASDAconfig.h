/*==============================================================================
Filename:	NASDAconfig.h

Description:	
	Contains default config settings for NASDAdirmon programs.
Includes names of config file, log file, source and destination hosts, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _NASDACONFIG_ 
#define _NASDACONFIG_

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
-- 	XXX_TRANSPROTO        - File transfer mechanism/FTP script
-- where XXX is NASDA
*/

/* NASDA default configuration
*/
#define NASDA_CONFIGFILE_DEF "NASDA/config/NASDAdirmon.config"
#define NASDA_LOGFILE_DEF    "NASDA/log/NASDAdirmon.log"

#define NASDA_DESTHOST_DEF   "" /* Ignored */
#define NASDA_TRANSPROTO_DEF "" /* Ignored */
#define NASDA_RECEPTDIR_DEF  "" /* Ignored */
#define NASDA_DESTDIR_DEF    "" /* Ignored */
#define NASDA_TRANSLDIR_DEF  "" /* Ignored */
#define NASDA_SRCDIR_DEF     "" /* Ignored */
#define NASDA_SRCHOST_DEF    "" /* Ignored */


Names_Table_Entry NASDA_Config_Names_Table[] =
{
   { CONFIG_FILE,       "NASDA_CONFIGFILE", NASDA_CONFIGFILE_DEF },
   { LOG_FILE,          "NASDA_LOGFILE",    NASDA_LOGFILE_DEF    },
   { SRC_DIR,           "NASDA_SRCDIR",     NASDA_SRCDIR_DEF     },
   { SRC_HOST,          "NASDA_SRCHOST",    NASDA_SRCHOST_DEF    },
   { DEST_DIR,          "NASDA_DESTDIR",    NASDA_DESTDIR_DEF    },
   { DEST_HOST,         "NASDA_DESTHOST",   NASDA_DESTHOST_DEF   },
   { RECEPT_DIR,        "NASDA_RECEPTDIR",  NASDA_RECEPTDIR_DEF  },
   { TRANSFER_PROTOCOL, "NASDA_TRANSPROTO", NASDA_TRANSPROTO_DEF },
   { ROOT_PATH,         "FAIF_ROOTPATH",    FAIF_ROOTPATH_DEF    },
   { TRANS_DIR,         "NASDA_TRANSLDIR",  NASDA_TRANSLDIR_DEF  },
   { SENTINEL,           NULL,              NULL                 }
} ;


#endif /* _NASDACONFIG_ */

/* End of File */
