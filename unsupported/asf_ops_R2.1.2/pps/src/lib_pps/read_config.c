/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	read_config.c

Description:	
	This contains the function which opens and reads a configuration
	file and stores the values in a global structure.

External Functions:
	read_config_file
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)read_config.c	1.2    12/16/96";

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPSerr.h"

PPSConfigStruct PPSConfigs[NUM_CONFIGS] =
{
	{	TRUE,	TRUE,	"PPS_USERID",	"" },
	{	TRUE,	TRUE,	"PPS_PASSWD",	"" },
	{	TRUE,	TRUE,	"PPS_SERVER",	"" },
	{	TRUE,	TRUE,	"PPS_DBNAME",	"" },
	{	TRUE,	TRUE,	"IMS_USERID",	"" },
	{	TRUE,	TRUE,	"IMS_PASSWD",	"" },
	{	TRUE,	TRUE,	"IMS_SERVER",	"" },
	{	TRUE,	TRUE,	"IMS_DBNAME",	"" },
	{	FALSE,	TRUE,	"NUM_THREADS",	"" },
	{	FALSE,	FALSE,	"AUTH_USERS",	"" }
};

extern char ProgName[MAXLINE];

/*==============================================================================
Function:	int read_config_file(config_file)

Description:	reads the configuration file and stores the values in the
		global structure PPSConfigs.

Notes:	 	This function returns ER_NO_ERROR if all configuration values
		are successfully read from the specified config file.  It
		returns ER_CONFIG_FILE if not all required configuration
		parameters are provided or the config file has bad format. 
==============================================================================*/
int read_config_file (char *configFile)
{
    char     logmsg[MAX_SYSLOG_MSGLEN+1] ;  /* Error log msg string */
    FILE     *fp=0;
    char     buf[MAXLINE];
    char     keyword[MAXLINE], value[MAXLINE];
    int      rc=0;
    int      k=0;
    int      ret;
    int	     specified_thread;
   
    /*---------------------------------------------*/
    /* configuration filename must be specified    */
    /* and readable                                */
    /*---------------------------------------------*/
    if (configFile == 0 || configFile[0] == '\0')
    {
        fprintf(stderr, "Configuration file not specified.  Bye...\n",
                          configFile);
        exit(1);
    }
    fp = fopen(configFile, "r");
    if (fp == 0)
    {
        fprintf(stderr, "%s: configuration file open failed.  Bye...\n",
                          configFile);
        exit(1);   
    }
        
    while (fgets(buf, MAXLINE-1, fp) != 0)
    {
        /*------------------------------*/
        /* takes '#' or '!' as comments */
        /*------------------------------*/
        if (*buf == '#' || *buf == '!')
		continue;

        /* read till end of line */
        rc = sscanf(buf, " %s %[^\n]", keyword, value);
        /* skip blank line */
        if (rc == EOF)
            continue;

        /*------------------------------*/
        /* must be keyword-value pair   */
        /*------------------------------*/
        if (rc != 2)
        {
            sprintf(logmsg, "Bad config file format [%s]", configFile);
            pps_logMsg(ProgName, PPS_ERROR, logmsg);
            fprintf(stderr,logmsg);
            fclose(fp);
            return (ER_CONFIG_FILE);
        }
        for (k=0; k < NUM_CONFIGS; k++)
        {
            if (strcmp(keyword, PPSConfigs[k].keyword) == 0)
            {
                if (PPSConfigs[k].isSingleValue)
					(void)sscanf(value, " %s", PPSConfigs[k].value);
				else
					strcpy(PPSConfigs[k].value, value);
            }
        } 
    }/*while*/

    fclose(fp);
    
    /* --------------------------------------------------------*/
    /* NUM_THREADS config parameter requires special handling  */
    /* because it's optional.                                  */
    /*---------------------------------------------------------*/
    
    /* if NUM_THREADS is not specified, use default value      */
    if (PPSConfigs[NUM_THREADS].value[0] == '\0')
    {
    	sprintf(PPSConfigs[NUM_THREADS].value,"%d", MAX_NUM_THREADS);
    }
    /* use default value if the specified NUM_THREADS is greater than */
    /* the maximum allowed */
    else
    {    
    	sscanf(PPSConfigs[NUM_THREADS].value, "%d", &specified_thread);
    	if (specified_thread > MAX_NUM_THREADS)
    		sprintf(PPSConfigs[NUM_THREADS].value, "%d", MAX_NUM_THREADS);
    }
    
    /*---------------------------------------------------------*/
    /* make sure all config values are set except NUM_THREADS  */
    /* because it's optional.                                  */
    /*---------------------------------------------------------*/
    for (k=0; k < NUM_CONFIGS; k++)
    {
        if (PPSConfigs[k].isRequired && PPSConfigs[k].value[0] == '\0')
        {
            sprintf(logmsg,"%s: Value of %s is not specified.\n",
            			configFile, PPSConfigs[k].keyword);
            pps_logMsg(ProgName, PPS_ERROR, logmsg);
            fprintf(stderr, logmsg);
            return(ER_CONFIG_FILE);
        }
        else if (PPSConfigs[k].value[0] != '\0')
        {
	        sprintf(logmsg,"%s is set to [%s]\n",
				PPSConfigs[k].keyword, PPSConfigs[k].value);
	        pps_logMsg(ProgName, PPS_INFO, logmsg); 
		    fprintf(stdout, logmsg);
	    }
    }
    return(ER_NO_ERROR);

} /* read_config_file */

/*---------------------------------------------*/
/* returns TRUE if the user name is in the     */
/* authorized name list in the config file;    */
/* else returns FALSE.                         */
/*---------------------------------------------*/
char
isAuthorizedUser(
char*	userName)
{
	char	authorizedUsers[MAXLINE];
	char	*subString;

	if (userName == 0)
		return FALSE;

	(void)strcpy(authorizedUsers, PPSConfigs[AUTH_USERS].value);
	for (subString=(char*)strtok(authorizedUsers, " ");
			subString; subString=(char*)strtok(0, " "))
	{
		if (strcmp(userName, subString) == 0)
			return TRUE;
	}

	return FALSE;

} /* isAuthorizedUser */
