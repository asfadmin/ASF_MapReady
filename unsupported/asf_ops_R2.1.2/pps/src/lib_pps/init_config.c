/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       init_config.c

Description:
        This module assigns the default values to variables which were
not set through the configuration file.

External Functions:
	check_config	
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)init_config.c	1.1    11/21/96";

#include <stdio.h>
#include "PPShdr.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "PPSerr.h"

extern CONFIG_STRUCT	g_config;


/*==============================================================================
Function:	void check_config()
Description:	Ensure that all config values are provided in the config file 	
Parameters:	None
Returns:	None
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 13:52:53 PDT 1995
Notes:		
==============================================================================*/
int check_config()
{
	char logmsg[MAX_SYSLOG_MSGLEN+1];		
	int ret = OK;

	if (g_config.num_threads == 0)
	{
                sprintf (logmsg,"Number of threads set to %d",
                         MAX_NUM_THREADS);
                pps_logMsg(ProgName,PPS_DEBUG,logmsg);
		g_config.num_threads = MAX_NUM_THREADS;
	}
	if (g_config.progname[0] == '\0')
		strcpy(g_config.progname,	APPLICATION_NAME);

	if (g_config.pps_userid[0] == '\0')
	{
		sprintf (logmsg,"%s keyword not found in configuration file",
			PPS_USERID_KEYWD);
		pps_logMsg(ProgName,PPS_ERROR,logmsg);
		ret = ER_INPUT_NULL;
	} 
	if (g_config.pps_passwd[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        PPS_PASSWD_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.pps_dbname[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        PPS_DBNAME_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.pps_server[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        PPS_SERVER_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.ims_userid[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        IMS_USERID_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.ims_passwd[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        IMS_PASSWD_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.ims_dbname[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        IMS_DBNAME_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }
	if (g_config.ims_server[0] == '\0')
        {
                sprintf (logmsg,"%s keyword not found in configuration file",
                        IMS_SERVER_KEYWD);
                pps_logMsg(ProgName,PPS_ERROR,logmsg); 
                ret = ER_INPUT_NULL;
        }

	return (ret);

} /* check_config */





/*==============================================================================
Function:	void init_config_null()
Description:	Initialize values for fields of global config record	
Parameters:	None
Returns:	None
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 13:53:57 PDT 1995
Notes:		
==============================================================================*/
void init_config_null()
{
	g_config.num_threads	= 0;
	g_config.progname[0]	= '\0';

	g_config.pps_userid[0]	= '\0';
	g_config.pps_passwd[0]	= '\0';
	g_config.pps_dbname[0]	= '\0';
	g_config.pps_server[0]	= '\0';

	g_config.ims_userid[0]	= '\0';
	g_config.ims_passwd[0]	= '\0';
	g_config.ims_dbname[0]	= '\0';
	g_config.ims_server[0]	= '\0';
}





/*==============================================================================
Function:	void set_num_threads (int num_threads)
Description:	Set number of threads field in global config record	
Parameters:	value to be assigned to global config record field
Returns:	None	
Creator:	YOURNAMEHERE
Creation Date:	Tue Oct 17 13:56:52 PDT 1995
Notes:		
==============================================================================*/
void set_num_threads (int num_threads)
{
	g_config.num_threads	= num_threads;
}





/*==============================================================================
Function:	int get_num_threads ()
Description:	Return value of num_threads in global config record	
Parameters:	None
Returns:	Value of num_threads field of global config record	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 13:58:33 PDT 1995
Notes:		
==============================================================================*/
int get_num_threads ()
{
	return (g_config.num_threads);
}


/* End of File */
