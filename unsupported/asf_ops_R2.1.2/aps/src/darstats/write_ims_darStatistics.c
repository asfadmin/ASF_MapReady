#undef RUN_FROM_GUI
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       write_ims_darStatistics.c

Description:    Routines to write to the IMS for DAR statistics.  

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          This file was written with a 4-character tab setting.
                If you don't use 4-character tabs, it will look funny.
                Use set tabstop=4 in vi to browse.  

==============================================================================*/
#pragma ident   "@(#)write_ims_darStatistics.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/darstats/SCCS/s.write_ims_darStatistics.c"

#undef TEST_ARGS_ONLY

#include <string.h>           /* for strcpy()   */
#include <stdlib.h>           /* for getenv()   */
#include "aps_defs.h"         /* for ENVVARS   */
#include "aps_log_msg.h"      /* for syslog   */

#include "aps_darStatistics.h"

char msg[MSG_LEN];



static void 
checkRetValue (int status,
                    IMS_MSG_STRUCT *msgDesc
                    )
{

    switch (status)
    {
        case IMS_OK:
            (void) ims_msg (msgDesc, status,
                "Successful completion.");
            break;

        case IMS_WARNING:
            (void) ims_msg (msgDesc, status,
                "");
            break;

        case IMS_ERROR:
            (void) ims_msg (msgDesc, status,
                "Input error");
            break;

        case IMS_FATAL:
            (void) ims_msg (msgDesc, status,
                "");
            break;

        default:
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Unknown query return status.");
            break;
      }
}
/***************************************************************
**
** display_error()
**
**************************************************************** */
 
static void 
display_error(IMS_MSG_STRUCT *msgDesc)
{
IMS_MSG_QUEUE *msgQueue;
  
      /* Extract and display the queued messages. */
    while ((msgQueue = ims_msgQueueExtract (msgDesc)) != (IMS_MSG_QUEUE *) NULL)
    {
          fprintf (stderr, "%s(%d):  %s\n", __FILE__, __LINE__, msgQueue->msg);
          ims_msgQueueFree (msgQueue);
    }
}
/***************************************************************
**
** checkQueryStatus ()
**
**************************************************************** */
static int 
checkQueryStatus ( 
    IMS_MSG_STRUCT *msgDesc, 
    IMS_CMN_QUERY *query,
    int status)
{
    switch (query->retStatus)
    {
        case IMS_OK:
            (void) ims_msg (msgDesc, status,
                "Error is not query related.");
            break;

        case IMS_NOCONNECT:
            (void) ims_msg (msgDesc, status,
                "Error due to connection failure.");
            break;

        case IMS_DEADLOCK:
            (void) ims_msg (msgDesc, status,
                "Error due to query deadlock.");
            break;

        case IMS_NOROWS:
            (void) ims_msg (msgDesc, status,
                "Error due to no rows returned.");
            break;

        case IMS_QUERYFAIL:
            (void) ims_msg (msgDesc, status,
                "Error due to query failure.");
            break;

        default:
            (void) ims_msg (msgDesc, IMS_ERROR,
                "Unknown query return status.");
            return (IMS_ERROR);
    }

    return (IMS_OK);
}   /*  checkQueryStatus   */

/*==============================================================================
Function:       write_ims_darStatistics()

Description:    writes to the IMS for DAR statistics.  

Creator:        Lawrence Stevens

Creation Date:  Thu Feb 22 16:36:22 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int 
write_ims_darStatistics(
    int dar_id,
    short item_id,
    char* dtk_status, 
    char* dar_timeStamp, 
    int dar_seconds,
    char* ims_username, 
    char* ims_password, 
    char* progName,
    IMS_MSG_STRUCT *msgDesc)
{
    IMS_CMN_QUERY       *query = NULL;
    IMS_DAR_STATISTICS  *darStats = NULL;
    int                 status;
    char                banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char                hostName[IMS_HOST_LEN+1];
    struct utsname      uname_info;     /*Structure for uname() */

    char                *ims_server = NULL ;
    char                *ims_database = NULL ;

#ifdef DEBUG
    printf("%s:(%d):  arguments to write_ims_darStatistics():\n", __FILE__, __LINE__);
    printf("\tdar_id        = %d\n", dar_id ) ;
    printf("\titem_id       = %hd\n", item_id ) ;
    printf("\tdtk_status    = %s\n", dtk_status ) ;
    printf("\tdar_timeStamp = %s\n", dar_timeStamp ) ;
    printf("\tdar_seconds   = %d\n", dar_seconds ) ;
    printf("\tims_username  = %s\n", ims_username ) ;
    printf("\tims_password  = %s\n", ims_password ) ;
    printf("\tprogName      = %s\n", progName ) ;
#endif

#ifdef TEST_ARGS_ONLY
    /* half the time, return OK.  */
    if ( rand() % 2 )
        return APS_EXIT_ERROR ;
    else
        return APS_EXIT_OK ;
#endif 

#ifndef TEST_ARGS_ONLY

    /* allocate space, init with zeros.  */
    query = (IMS_CMN_QUERY *) calloc(1, sizeof( IMS_CMN_QUERY ));
    if ( query == NULL )
    {
        sprintf(msg,
            "IMS query structure IMS_CMN_QUERY allocation failed." ) ;
#ifdef RUN_FROM_GUI 
        fprintf(stderr, "%s(%d):  %s:\n\n%s\n", __FILE__, __LINE__, 
            progName, msg ) ;
#endif /*  RUN_FROM_GUI    */
        aps_log_msg(progName, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progName);
    }

    darStats = (IMS_DAR_STATISTICS *) malloc( sizeof(IMS_DAR_STATISTICS));

    if ( darStats == NULL )
    {
        sprintf(msg,
            "IMS darStats structure IMS DAR STATISTICS allocation failed." ) ;
#ifdef RUN_FROM_GUI 
        fprintf(stderr, "%s(%d):  %s:\n\n%s\n", __FILE__, __LINE__, 
            progName, msg ) ;
#endif /*  RUN_FROM_GUI    */
        aps_log_msg(progName, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        error_exit(progName);
    }

    /*
    ** Get the node name.
    */
    (void) uname (&uname_info);
    (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
    hostName[IMS_HOST_LEN] = '\0';

    /*
    ** Initialize the message facility options.
    */
    (void) ims_msgSubSystem (msgDesc, "APS");
    (void) ims_msgProgramName (msgDesc, progName);
    (void) sprintf (banner, "%s::%s", hostName, progName);
    (void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
    (void) ims_msgOpenSyslog (msgDesc, "APS:", LOG_LOCAL1);
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

    darStats-> order_id = dar_id;
    darStats-> item_id = item_id;
    darStats-> seconds = dar_seconds;
    (void) strcpy (darStats->time_stamp, dar_timeStamp);
    (void) strcpy (darStats->status, dtk_status);

    (void) strcpy (query->username, ims_username);
    (void) strcpy (query->password, ims_password);
    (void) strcpy (query->program, progName);
    query->msgDesc = msgDesc;

    ims_server = getenv(IMS_SERVER_ENVVAR);

    if ( ims_server == NULL )
    {
        sprintf(msg, "%s environment variable not set.", IMS_SERVER_ENVVAR );
        aps_log_msg(progName, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }
    else
        strcpy( query->server, ims_server ) ;

    ims_database = getenv(IMS_DB_ENVVAR) ;
    if ( ims_database == NULL )
    {
        sprintf(msg, "%s environment variable not set.", IMS_DB_ENVVAR );
        aps_log_msg( progName, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }
    else
        strcpy( query->database, ims_database ) ;

    /*  open the connection */
    status = ims_openQueryConnection( query );
    if(  status  <  IMS_OK )
    {
        (void) checkQueryStatus(msgDesc, query, status);
        if (query != NULL)
            free(query);
        if (darStats != NULL)
            free(darStats);
        return APS_EXIT_ERROR ;
    } 

    /*
    ** Start queuing messages.
    */
    (void) ims_msgStderrFlag(msgDesc, IMS_OFF);
    (void) ims_msgQueueFlag(msgDesc, IMS_ON);

    /*
    ** Perform the query.
    */
    status = ims_darStatistics(query, darStats ) ;
    if ( status < IMS_OK)
    {
        (void) checkRetValue(status, msgDesc);

/*
        (void) checkQueryStatus (msgDesc, query, status);
*/
        /*  close query connection */
        if (ims_closeQueryConnection(query) < IMS_OK)
        {
            (void) checkQueryStatus (msgDesc, query, status);
        } 
        display_error(msgDesc);
        if (query != NULL)
            free(query);
        if (darStats != NULL)
            free(darStats);
        return APS_EXIT_ERROR ;
    }

    /*
    -- comment out display_error() for OK return codes.  
    --
    (void) checkRetValue(status, msgDesc);
    display_error(msgDesc);
    */

    /*  close query connection */
    if ((status = ims_closeQueryConnection(query)) < IMS_OK)
    {
        (void) checkQueryStatus (msgDesc, query, status);
    } 

    if (query != NULL)
        free(query);
    if (darStats != NULL)
        free(darStats);

    return APS_EXIT_OK ;

#endif   /* ifndef TEST_ARGS_ONLY */

} /* write_ims_darStatistics */
