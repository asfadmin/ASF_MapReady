/*==============================================================================
Filename:	cron_cfg.h

Description:	
	Crontab command specification and data structure definition for 
Cron Config record.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CRONCFG_
#define _CRONCFG_

#define CRONTAB_PATH "/usr/bin/crontab"
#define CRONTAB_CMD "crontab"
 
#define TMP_CRON_SCRIPT_DIR "CRONTMP"

#ifdef ESA_DM
#define TMP_CRON_SCRIPT     "ESAcron_env.sh"

#elif NASDA_DM
#define TMP_CRON_SCRIPT     "NASDAcron_env.sh"

#elif WALPS_DM
#define TMP_CRON_SCRIPT     "WALPScron_env.sh"

#elif ADEOS_DM
#define TMP_CRON_SCRIPT     "ADEOScron_env.sh"

#elif CSA_DM
#define TMP_CRON_SCRIPT     "CSAcron_env.sh"

#endif


typedef struct cron_config_rec
{
   int minute ;
   int hour ;
   int monthday ;
   int month ;
   int weekday ;
   char *action_cmd ;
   char *crontab_file ;
   char *cron_cmd ;
   char *rootpath ;
   char *env_script ;

} Cron_Config_Record ;

#endif /* _CRONCFG_ */

/* End of File */
