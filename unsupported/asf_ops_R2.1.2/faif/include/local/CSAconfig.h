/*==============================================================================
Filename:	CSAconfig.h

Description:	
	Contains default config settings for CSAgetfile program.  Includes
names of config file, log file, source and destination hosts, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSATABLES_
#define _CSATABLES_

#include "configrec.h"     /* Definition of Config record */

/* Config/Environment Variable Names
--
-- A subset of names of environment variables that can be set to
-- specify the configuration of a directory monitor are
-- listed below.  
--
--      FAIF_ROOTPATH    - Root pathname for reception directory,
--                         config file, log file.
--      XXX_CONFIGFILE   - Name of config file
--      XXX_LOGFILE      - Directory monitor log file
--      XXX_SRCHOST      - Incoming files machine
--      XXX_DESTHOST     - Files destination machine
--      XXX_SCRIPT_NAME  - name of FTP script used to pull files
--                         from the CSA remote host
-- where XXX is CSA
-- 
-- Other config variable names are found in CSArouter.h
--
*/


/* CSA default configuration
*/
#define CSA_CONFIGFILE_DEF "CSA/config/CSAgetfile.config"
#define CSA_LOGFILE_DEF    "CSA/log/CSAgetfile.log"
#define CSA_SRCHOST_DEF    "mcs_vdn"

#define CSA_DESTHOST_DEF   ""  /* Ignored */
#define CSA_TRANSPROTO_DEF ""  /* Ignored */
#define CSA_SRCDIR_DEF     ""  /* Ignored */
#define CSA_DESTDIR_DEF    ""  /* Ignored */
#define CSA_RECEPTDIR_DEF  ""  /* Ignored */
#define CSA_TRANSLDIR_DEF  ""  /* Ignored */

#define CSA_GETFILE_SCRIPT_EV   "CSA_GET_SCRIPT"
#define CSA_GETFILE_SCRIPT_DEF  "CSA_ftpget.csh"

/* Table of CSAgetfile configuration variables
-- with associated default values
*/
Names_Table_Entry CSA_Config_Names_Table[] =
{
   { CONFIG_FILE,       "CSA_CONFIGFILE", CSA_CONFIGFILE_DEF },
   { LOG_FILE,          "CSA_LOGFILE",    CSA_LOGFILE_DEF    },
   { SRC_DIR,           "CSA_SRCDIR",     CSA_SRCDIR_DEF     },
   { SRC_HOST,          "CSA_SRCHOST",    CSA_SRCHOST_DEF    },
   { DEST_DIR,          "CSA_DESTDIR",    CSA_DESTDIR_DEF    },
   { DEST_HOST,         "CSA_DESTHOST",   CSA_DESTHOST_DEF   },
   { RECEPT_DIR,        "CSA_RECEPTDIR",  CSA_RECEPTDIR_DEF  },
   { TRANSFER_PROTOCOL, "CSA_TRANSPROTO", CSA_TRANSPROTO_DEF },
   { ROOT_PATH,         "FAIF_ROOTPATH",  FAIF_ROOTPATH_DEF  },
   { TRANS_DIR,         "CSA_TRANSLDIR",  CSA_TRANSLDIR_DEF  },
   { SENTINEL,           NULL,            NULL               }
} ;

#endif /* _CSATABLES_ */

/* End of file */
