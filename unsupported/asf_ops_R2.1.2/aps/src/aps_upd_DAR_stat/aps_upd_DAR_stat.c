#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       
 
Description:    aps_upd_DAR_stat.c
 
External Functions Defined:
                        static void set_ims_aps_progName()
						static int set_ims_query_struct()
						static int checkQueryStatus()
						void free_imsMsgStruct()
						int aps_darStatus()
						static void log_ims_error()
						int aps_upd_DAR_stat()

External Variables Defined:
						extern char				*progname ;
						extern char    			*sybase_userid ;
						extern char    			*password ;

File Scope Variables:
        
Notes:
 
==============================================================================*/
#pragma ident   "@(#)aps_upd_DAR_stat.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_upd_DAR_stat/SCCS/s.aps_upd_DAR_stat.c"

#include <stdio.h>
#include <ims_query.h>		/* for IMS_MSG_STRUCT, etc.                   */
#include <stdlib.h>         /* for exit() etc.                            */
#include <string.h>         /* for strcmp() etc.                          */
#include <db_sybint.h>      /* for DBPROCESS                              */
#include <timeconv.h>       /* for tc_validate_asf_datetime() etc.        */
#include <db_dar.h>         /* for CAST_DAR_SAT etc.                      */
#include <aps_db_table.h>   /* for DAR, DTK                               */
#include <aps_log_msg.h>    /* for APS_CRITICAL, etc.                     */
#include <mu_utilities.h>   /* for MU_DAR_ACTIVITY_TYPE, etc.             */
#include <sys/utsname.h>

#include "aps_upd_DAR_stat.h"
#include "aps_defs.h"
#include "DARconversions.h"

static IMS_CMN_QUERY	*imsdb_query = NULL;
static char				*ims_aps_progName = NULL ;

extern char				*progname ;
extern char    			*sybase_userid ;
extern char    			*password ;


/*==============================================================================
Function:       set_ims_aps_progName

Description:    sets the "constant" program name for ims DAR API calls

Parameters:     

Returns:        

Creator:        Your_Name

Creation Date:  03/15/96

Notes:		
==============================================================================*/
static void
set_ims_aps_progName()
{
	ims_aps_progName = (char *) malloc( strlen( progname ) 
			+ strlen( APS_IMS_PROG_SUFFIX ) + 1 ) ;
	(void) sprintf( ims_aps_progName, "%s%s",
			progname, APS_IMS_PROG_SUFFIX ) ;

	return ;
}



/*==============================================================================
Function:       set_ims_query_struct

Description:    If not already set, allocates space for and sets up the
				non-changing items in the query struct used in the
				IMS API calls.  If any errors occur a popup_message is
				displayed.

Parameters:     

Returns:        IMS_OK
				IMS_ERROR indicating a problem occurred

Creator:        Quentin Sun's code modified by Teresa McKillop

Creation Date:  01/30/96

Notes:			sets everything except:
					query->msgDesc
					query->retStatus
					query->retPtr

				query->qDesc is init. to NULL for ims_openQueryConnection()
==============================================================================*/

static int
set_ims_query_struct( IMS_MSG_STRUCT *msgDesc )
{
	char			*imsServer;
	char			*imsDB;

	if (!ims_aps_progName)
		set_ims_aps_progName() ;

	/*
	** setup the imsdb_query structure
	*/

	if (imsdb_query != NULL)
		free( imsdb_query );
	imsdb_query = (IMS_CMN_QUERY *) calloc( 1, sizeof( IMS_CMN_QUERY ));
		
    (void) strcpy( imsdb_query->username, sybase_userid );
    (void) strcpy( imsdb_query->password, password );
    (void) strcpy( imsdb_query->program,  ims_aps_progName );


	if ((imsServer = getenv( APS_IMS_SERVER_ENVVAR )) != NULL)
		(void) strcpy( imsdb_query->server, imsServer );
	else
	{
		(void) ims_msg( msgDesc, IMS_ERROR,
			"IMS_SERVER environment variable not set." );
		return (IMS_ERROR);
	}

	if ((imsDB = getenv( APS_IMS_DB_ENVVAR )) != NULL)
		(void) strcpy( imsdb_query->database, imsDB );
	else
	{
		(void) ims_msg( msgDesc, IMS_ERROR,
			"IMS_DB environment variable not set." );
		return (IMS_ERROR);
	}

	return (IMS_OK);
}



/*==============================================================================
Function:       checkQueryStatus

Description:    

Parameters:     

Returns:        the return status from the query or IMS_ERROR if it
				is not identifiable.

Creator:        Quentin Sun

Creation Date:  01/30/96

Notes:		
==============================================================================*/
static int
checkQueryStatus ( IMS_MSG_STRUCT *msgDesc, IMS_CMN_QUERY *query, int status )
{
	int		retStatus;

	retStatus = query->retStatus;

    switch (retStatus)
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

    return (retStatus);
}   /*  checkQueryStatus   */



/*==============================================================================
Function:       free_imsMsgStruct

Description:    frees the ims msgDesc structure and does any necessary
				resetting.

Parameters:     imsMsgStruct	- the structure to be freed

Returns:        the return status from the query or IMS_ERROR if it
				is not identifiable.

Creator:        Teresa McKillop

Creation Date:  01/30/96

Notes:		
==============================================================================*/
void
free_imsMsgStruct( IMS_MSG_STRUCT **imsMsgStruct )
{
	(void) ims_msgStructFree( *imsMsgStruct );
	*imsMsgStruct = NULL;

	/* need to fix the APS db because IMS messed up err handling */
	(void) db_install_message_handler( db_default_message_handler ) ;
	(void) db_install_error_handler( error_handler_exit ) ;

	return;
}



/*==============================================================================
Function:       aps_darStatus

Description:    does all the IMS setup processing, calls ims_darStatus,
				then does all the IMS close processing

Parameters:     apsStatus	- APS status value
				apsPlnrCmnt	- APS dar planner comments
				msgDesc		- IMS's message struct

Returns:        >= IMS_OK	- IMS found rows matching the query
				all others	- an erroneous condition occurred

Creator:        Teresa McKillop

Creation Date:  02/02/96

Notes:		
==============================================================================*/
int
aps_darStatus( int darid, char *apsStatus, char *apsPlnrCmnt,
	IMS_MSG_STRUCT *msgDesc )
{
	IMS_DAR_STATUS	ims_dar_status;	/* struct for updating the IMS db DARs */

	char			*imsStatus = NULL;

    int				status;

	/*
	-- Set up the IMS darStatus structure
	*/

	ims_dar_status.order_id = darid;
	ims_dar_status.item_id	= DEF_IMS_DAR_ITEM_ID;	/* always is this */
	if ((imsStatus = table_lookupAPS2IMS( dar_status,
		apsStatus, NULL )) == NULL)
	{
		imsStatus = dar_status[0].ims_value;
	}
	(void) strcpy( ims_dar_status.status, imsStatus );
	(void) strcpy( ims_dar_status.planner_comment, apsPlnrCmnt );

	/*
	** Set up the query structure.
	*/

	if ((status = set_ims_query_struct( msgDesc )) < IMS_OK)
	{
		return( status );	/* error message is in IMS message struct */
	}

	imsdb_query->msgDesc    = msgDesc;
	imsdb_query->retStatus  = IMS_OK;

	/*  connect to the IMS server */
	status = ims_openQueryConnection( imsdb_query );
    if(  status  <  IMS_OK )
    {
        (void) checkQueryStatus( msgDesc, imsdb_query, status );
		if (imsdb_query != NULL)
		{
			free(imsdb_query);
			imsdb_query = NULL;
		}
        return(status);
    } 

    /*
    ** Perform the update.
    */

    if ((status = ims_darStatus( imsdb_query, &ims_dar_status )) < IMS_OK)
    {
		(void) checkQueryStatus (msgDesc, imsdb_query, status);
		/*  close query connection */
		if (ims_closeQueryConnection( imsdb_query ) < IMS_OK)
		{
			(void) checkQueryStatus (msgDesc, imsdb_query, status);
		} 
	}
	/*  else, close query connection */
	else if ((status = ims_closeQueryConnection(imsdb_query)) < IMS_OK)
    {
        (void) checkQueryStatus (msgDesc, imsdb_query, status);
    } 

	if (imsdb_query != NULL)
	{
		free(imsdb_query);
		imsdb_query = NULL;
	}

    return(status);

} /* aps_darStatus */



/*==============================================================================
Function:       log_ims_error()

Description:    Logs an error message made up of the IMS error
				message portion of the IMS_MSG_QUEUE argument,
				preceded by msgStr, if it is not NULL.

Parameters:     progname		- name of the executing program
				msgStr          - a string to prepend to the msgStruct msg's
				msgStruct       - message structure set during an IMS call

Returns:        N/A

Creator:        Philip Yurchuk (inspired by Teresa McKillop)

Creation Date:  8-18-97

==============================================================================*/
static void
log_ims_error( char *progname, char *msgStr, IMS_MSG_STRUCT *msgStruct )
{
  	IMS_MSG_QUEUE   *msgQueue;
	int             msgLength;
	char            buf[1024];
	
	if (msgStruct != NULL)
	{
	  	/* Extract and display the queued messages. */
	  	if ((msgLength = (msgStr == NULL) ? 0 : strlen( msgStr )) > 0)
		  	(void) strcpy( buf, msgStr );

		while ((msgQueue = ims_msgQueueExtract( msgStruct )) != NULL)
		{
		  	if ((msgLength += (int) strlen( msgQueue->msg ))
				>=  1024)
			{
			  	break;
			}
			(void) strcat( buf, msgQueue->msg );
			ims_msgQueueFree (msgQueue);
		}
	}
	aps_log_msg(progname, APS_ERROR, buf, DO_SYSLOG, DO_PRINT);

	return;
}


/*==============================================================================
Function:       aps_upd_DAR_stat()

Description:    Deletes DARs in both the APS and IMS DAR relations.  

Returns:        1 if DARs were deleted.  
                0 if no DARs were deleted - no errors in processing.  
                < 0 if an error was encountered.  

Creator:        Philip Yurchuk

Creation Date:  8-8-97

==============================================================================*/
int aps_upd_DAR_stat(char *progname, DBPROCESS *APS_dbproc, char *sat, 
					 char *start_time, char *stop_time, char *comment, 
					 int no_perms)
{
    /* 
    -- NOTE:  care must be taken to always clean up before 
    --        returning.  Permissions may need to be terminated,
    --        memory may need to be freed.  
    --        Be really careful about this.  
    */
    int           mu_return_code ;

    DB_RECORD   **dar_rec ;
    llist       *dar_list ;
    cursor      dar_list_ptr ;
    llist       *perm_list = NULL ;

    char        buf[256] ;
	char		tmpstr[1024] ;

	IMS_MSG_STRUCT  *imsMsgStruct;
    char            banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char            hostName[IMS_HOST_LEN+1];

    struct utsname  uname_info;     /* Structure for uname() */
	char 	plnrcmnt[255];

	int			dar_id ;

    /* create the where clause */
    (void) sprintf(where_clause, "where %s = '%s' and %s >= '%s' and %s <= '%s'",
		   APS_COL(DAR, DAR_SAT), sat,
		   APS_COL(DAR, DAR_STRTTIME), start_time,
		   APS_COL(DAR, DAR_ENDTIME), stop_time) ;

    dar_list = db_get_records(APS_dbproc, APS_TABLE(DAR),
        where_clause, NULL, APS_CDEFS(DAR), ALL_COLS) ;

    if (dar_list == NULL)
        return APS_UPD_DAR_STAT_ERROR_DB_QUERY_ERROR ;

    if( NUMELTS( dar_list ) <= 0 )
    {
        DEL_LIST( dar_list ) ;
		(void) sprintf( buf, "No DARs were found for the given parameters." ) ;
		aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);
        return APS_UPD_DAR_STAT_DAR_NOT_FOUND ;
    }

    /* Process the list of DARs */
    dar_rec = FIRST(dar_list, dar_list_ptr) ;

    while (dar_rec)
    {
		perm_list = create_dyn_llist() ;
		dar_id = CAST_DAR_DARID  dar_rec[DAR_DARID] ;

		/* if no_perms > 0, then we don't bother to get permission */
		if (no_perms <= 0)
		{
        	mu_return_code = mu_permission_request(APS_dbproc,
												   MU_EMPTY_INT_PARAM,
												   MU_EDIT_OR_DELETE_DAR,
												   MU_DAR_ACTIVITY_TYPE,
												   perm_list,
												   NULL, NULL, NULL,
												   dar_id) ;

			if (mu_return_code < 0)
			{
	    		(void)sprintf(buf, "%s(%d):  request planning error code %d\n", 
							  __FILE__, __LINE__, mu_return_code ) ;
				aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);
				DEL_LIST( dar_list ) ;
				DEL_LIST( perm_list ) ;
				return (APS_UPD_DAR_STAT_ERROR_PERMISSION_ERROR) ;
			}
			else if (mu_return_code == 0 )
        	{
            	(void)sprintf(buf, "%s(%d):  request planning DENIED \n", 
							  __FILE__, __LINE__ ) ;
				aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);
				
				(void)printf("%s(%d):  Blocking planning perms:\n", __FILE__, __LINE__ ) ;
				db_print_list( perm_list, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
				
				DEL_LIST( dar_list ) ;
				DEL_LIST( perm_list ) ;

				return (APS_UPD_DAR_STAT_ERROR_PERM_NOT_GRANTED) ;
        	}

			/* Permission granted - continue */
			DEL_LIST( perm_list ) ;
		}

		/* Allocate and initialize the IMS msgDesc structure */
        if ((imsMsgStruct = ims_msgStructAlloc()) == (IMS_MSG_STRUCT *) NULL)
        {
        	(void) sprintf( buf,
						   "Can't begin IMS/DADS operation\n Too many processes running?" );
			aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);

			DEL_LIST( dar_list ) ;

			return (APS_UPD_DAR_STAT_ERROR_NO_IMS_MSG);
        }
		
		if (!ims_aps_progName)
		  set_ims_aps_progName() ;
 
		(void) uname (&uname_info);
		(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
		hostName[IMS_HOST_LEN] = '\0';
		
		(void) ims_msgSubSystem (imsMsgStruct, APS_SUBSYS_NAME);
		(void) ims_msgProgramName (imsMsgStruct, ims_aps_progName);
		(void) sprintf (banner, "%s::%s", hostName, ims_aps_progName);
		(void) ims_msgBanner (imsMsgStruct, banner, IMS_MSG_ALLBANNER);
		(void) ims_msgOpenSyslog (imsMsgStruct, "APS:", LOG_LOCAL1);
		(void) ims_msgSybErrHndlFlag (imsMsgStruct, IMS_ON);
		(void) ims_msgSybMsgHndlFlag (imsMsgStruct, IMS_ON);
		 
		/* queue the IMS messages. */
		(void) ims_msgStderrFlag( imsMsgStruct, IMS_OFF );
		(void) ims_msgQueueFlag( imsMsgStruct, IMS_ON );

		/* 
		-- Append the comment given in the command line, if one was given. 
		-- Just don't forget there's a max. of 255 characters in plnrcmnt.
		*/

		if (comment)
		{
		  	(void) strcpy(tmpstr, CAST_DAR_PLNRCMNT dar_rec[DAR_PLNRCMNT]) ;
			(void) strcat(tmpstr, "\n") ;
		  	(void) strcat(tmpstr, comment) ;
			(void) strncpy(plnrcmnt, tmpstr, 255) ;
		}

		if (aps_darStatus( dar_id, APS_REJECTED_STATUS, plnrcmnt, 
						  imsMsgStruct ) 
			< IMS_OK)
		{
		  	/* couldn't update the IMS DAR, do NOT delete from the APS DB */
		    (void) sprintf(buf,
						   "ERROR can't update IMS/DADS DAR status\n APS DAR %d is not deleted\n",
						   dar_id );
			log_ims_error(progname, buf, imsMsgStruct );
			
			DEL_LIST( dar_list ) ;
			free_imsMsgStruct( &imsMsgStruct );
			return (APS_UPD_DAR_STAT_ERROR_IMS_FAILURE) ;
		}
		else    /* updated in IMS DAR, ie, status: Completed or Rejected */
		{
		  	free_imsMsgStruct( &imsMsgStruct );
			
			/* IMS DAR is updated, now delete the DAR from the APS db */
			(void) sprintf( where_clause,
						   "where %s = %d", APS_COL(DAR, DAR_DARID ), dar_id ) ;
			if (db_delete_records( APS_dbproc, APS_TABLE(DAR), where_clause )
				!= 1)
			{
			  	/* DBs not in sync: can't delete from APS, IMS shows it's done*/
			  	(void) sprintf( buf,
							   "APS DB Error:\n can't delete DAR %d\n IMS/DADS db shows delete is done\n (fix APS db, then delete again)",
							   dar_id );
				aps_log_msg(progname, APS_INFO, buf, DO_SYSLOG, DO_PRINT);

			}
		}
		dar_rec = NEXT(dar_list, dar_list_ptr) ;
	}
	
	DEL_LIST( dar_list ) ;
	
	return (APS_UPD_DAR_STAT_OK) ;
}

