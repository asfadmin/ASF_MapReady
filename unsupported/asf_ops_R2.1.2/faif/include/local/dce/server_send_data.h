/*==============================================================================
Filename:	server_init.h

Description:
	Server init header file contains #define's for default DCE CDS
(Cell Directory Service) entry name for the FAxmitserver, default config 
file, and name of environment variable that can be used to set the name of
the config file.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _SERVERINIT_
#define _SERVERINIT_

/* Name of Environment variable that can be used to specify the
-- server entry name of the server in CDS.  
*/
#define FAXMIT_CDS_ENTRY_EV "FAXMIT_CDS_ENTRY"

/* Name of default config filename and name of environment variable
-- for config filename.  Note that the actual config filename used
-- is the FAIF rootpath plus the value specified here.  ie. if rootpath
-- is "/local/faif" and config file is "config/myconfig",
-- the actual config file is stored in "/local/faif/config/myconfig".
*/
#define CONFIG_FILE_DEF  "config/FAxmitserver.config"
#define CONFIG_FILE_EV   "FAXMIT_CONFIG"

/* FAxmitserver temporary directory is where files sent
-- by clients are copied into and then sent to remote FA hosts
-- Note that the actual temp. dir. used is the FAIF rootpath plus 
-- the value specified here.  ie. if rootpath is "/local/faif" 
-- and temp. dir. is "FTP/stmp", the actual temp. dir. used is
-- "/local/faif/FTP/stmp".
*/
#define FAXMIT_TMPDIR_EV  "FAXMIT_TMPDIR"
#define FAXMIT_TMPDIR_DEF "FTP/stmp"

#endif /* _SERVERINIT_ */

/* End of File */
