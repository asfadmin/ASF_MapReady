#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		sv_query.c

Description:	Perform queries on state vector data as requested
				by the caller and return a pointer to a link list
				of all records retreived

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)sv_query.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/create_sv_files/SCCS/s.sv_query.c"

#include "create_sv_file.h"
#include "aps_log_msg.h"

/* Local functions */
static int checkQueryStatus (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, int);

/*
void display_error(IMS_MSG_STRUCT *msgDesc);
*/

IMS_SV_STRUCT * sv_query(COMMANDS *commands)
{
    int status;
    char odl_start_time[SV_TIME_LENGTH + 1];
    char odl_end_time[SV_TIME_LENGTH + 1];
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;     /*Structure for uname() */
    
    IMS_MSG_STRUCT *msgDesc;
    IMS_CMN_QUERY *query;
    static IMS_SV_STRUCT retbuf;
    short listResultsFlag = IMS_TRUE;

	/*
	-- PR 2460 fix: We no longer allocate memory using 'malloc', but instead
	-- use 'calloc' which initializes the memory to zero.  This guarantees
	-- that the members of the IMS_CMN_QUERY structure remain NULL until a
	-- value is assigned to them.
	-- (The qDesc member needs to be NULL before ims_openQueryConnection
	-- routine is called; a non-NULL value causes a core dump to occur)
	*/
    query = (IMS_CMN_QUERY *) calloc(1, sizeof( IMS_CMN_QUERY ) );

    /*
    ** Get the node name.
	*/
    (void) uname (&uname_info);
    (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
    hostName[IMS_HOST_LEN] = '\0';

    /*
    ** Allocate message facility structure.
    */
    if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
    {
		aps_log_msg(commands->progname, APS_ERROR, 
            		"Memory allocation for IMS_MSG_STRUCT structure failed.", 
					DO_SYSLOG, DO_PRINT);
		if (query != NULL)
			free(query);
		return(NULL);
    }

    /*
    ** Initialize the message facility options.
    */
    (void) ims_msgSubSystem (msgDesc, "APS");
    (void) ims_msgProgramName (msgDesc, commands->progname);
    (void) sprintf (banner, "%s::%s", hostName, commands->progname);
    (void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
    (void) ims_msgOpenSyslog (msgDesc, "APS:", LOG_LOCAL1);
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

   	/*  
	** Initialize and open the query connection. 
	*/
    (void) strcpy (query->username, commands->username);
    (void) strcpy (query->password, commands->password);
    (void) strcpy (query->program, commands->progname);
    strcpy( query->server, commands->server);
    strcpy( query->database, commands->database);
    query->msgDesc = msgDesc;
    query->retPtr = (char *) &retbuf;
	status = ims_openQueryConnection( query );
    if(  status  <  IMS_OK )
    {
    	ims_msgStructFree (msgDesc);
		aps_log_msg(commands->progname, APS_ERROR, 
            	"Could not open IMS query connection.", DO_SYSLOG, DO_PRINT);
		if (query != NULL)
			free(query);
        return(NULL);
    } 

    /*
    ** Turn off message queuing.
    */
    (void) ims_msgStderrFlag (msgDesc, IMS_ON);
    (void) ims_msgQueueFlag (msgDesc, IMS_OFF);

    /*
    ** Perform the query.
    */

    /* convert asf time to odl time */
    tc_asf2odl(commands->startTime,odl_start_time);
    tc_asf2odl(commands->endTime,odl_end_time);

    if ((status = ims_svQuery(query,commands->platform,commands->precision[0],
        odl_start_time, odl_end_time, listResultsFlag)) < IMS_OK)
    {
        (void) checkQueryStatus (msgDesc, query, status);
        aps_log_msg(commands->progname, APS_ERROR, 
					"Could not query IMS state vectors data.", 
					DO_SYSLOG, DO_PRINT);

        /*  close query connection */
        status = ims_closeQueryConnection( query );
        if(  status  <  IMS_OK )
        {
        	aps_log_msg(commands->progname, APS_ERROR, 
						"Could not close IMS query connection.", 
						DO_SYSLOG, DO_PRINT);
        } 

		/*
    	** Shutdown the message facility.
    	*/
    	(void) ims_msgStructFree (msgDesc);
		if (query != NULL)
			free(query);
        return(NULL);
    }

   	/*  close query connection */
    status = ims_closeQueryConnection( query );
    if(  status  <  IMS_OK )
    {
       	aps_log_msg(commands->progname, APS_ERROR, 
				"Could not close IMS query connection.", DO_SYSLOG, DO_PRINT);
    } 
	/*
    ** Shutdown the message facility.
    */
    (void) ims_msgStructFree (msgDesc);

	if (query != NULL)
		free(query);

    return(&retbuf);

} /* sv_query */

/***************************************************************
**
** checkQueryStatus ()
**
**************************************************************** */

static int checkQueryStatus (
    IMS_MSG_STRUCT *msgDesc,
    IMS_CMN_QUERY *query,
    int status)
{
char msg[MSG_LEN];

    switch (query->retStatus)
    {
        case IMS_OK:
       		aps_log_msg(query->program, APS_INFO, 
                		"Error is not query related.",
						DO_SYSLOG, DO_PRINT);
            break;

        case IMS_NOCONNECT:
       		aps_log_msg(query->program, APS_ERROR, 
                		"Error due to connection failure.",
						DO_SYSLOG, DO_PRINT);
            break;

        case IMS_DEADLOCK:
       		aps_log_msg(query->program, APS_ERROR, 
                		"Error due to query deadlock.",
						DO_SYSLOG, DO_PRINT);
            break;

        case IMS_NOROWS:
       		aps_log_msg(query->program, APS_WARNING, 
                		"Error due to no rows returned.",
						DO_SYSLOG, DO_PRINT);
            break;

        case IMS_QUERYFAIL:
       		aps_log_msg(query->program, APS_ERROR, 
                		"Error due to query failure.",
						DO_SYSLOG, DO_PRINT);
            break;

        default:
			sprintf(msg, "Unknown IMS query return status, '%d'", 
					query->retStatus);
       		aps_log_msg(query->program, APS_WARNING, msg, 
						DO_SYSLOG, DO_PRINT);
            return (IMS_ERROR);
    }

    return (IMS_OK);
}   /*  checkQueryStatus   */


 
/***************************************************************
**
** display_error()
**
*****************************************************************/
/* 
void display_error(IMS_MSG_STRUCT *msgDesc)
{
IMS_MSG_QUEUE *msgQueue;

    while ((msgQueue = ims_msgQueueExtract (msgDesc)) != (IMS_MSG_QUEUE *) NULL)
	{
   		fprintf (stderr, "%s\n", msgQueue->msg);
        ims_msgQueueFree (msgQueue);
	}
}
*/
