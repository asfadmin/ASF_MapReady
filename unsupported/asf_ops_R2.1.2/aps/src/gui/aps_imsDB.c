#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps_imsDB.c

Description:	Query DAR data from IMS.

External Functions Defined:
				aps_darQuery
				aps_darStatus
				alloc_imsMsgStruct
				free_imsMsgStruct
	
File Scope Functions:
				set_ims_aps_progName
				set_ims_query_struct
				checkQueryStatus
	
External Variables Defined:
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)aps_imsDB.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.aps_imsDB.c"

#include <stdlib.h>		/* needed for malloc() */

#include <Xm/Xm.h>

#include "DARconversions.h"

#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_imsDB.h"


extern void				popup_message();

extern char				display_string[] ;

static IMS_CMN_QUERY	*imsdb_query = NULL;
static char				*ims_aps_progName = NULL ;



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
	ims_aps_progName = (char *) malloc( strlen( APS_progname ) 
			+ strlen( APS_IMS_PROG_SUFFIX ) + 1 ) ;
	(void) sprintf( ims_aps_progName, "%s%s",
			APS_progname, APS_IMS_PROG_SUFFIX ) ;

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
		
    (void) strcpy( imsdb_query->username, userid );
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
Function:       alloc_imsMsgStruct

Description:    allocates and initializes the ims msgDesc structure.

Parameters:     

Returns:        the initialized msgDesc structure
				OR NULL if the structure was not allocated

Creator:        Teresa McKillop

Creation Date:  02/02/96

Notes:		
==============================================================================*/
IMS_MSG_STRUCT *
alloc_imsMsgStruct ()
{
	IMS_MSG_STRUCT *imsMsgStruct;
    char			banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char			hostName[IMS_HOST_LEN+1];
    struct utsname	uname_info;     /*Structure for uname() */

	if ((imsMsgStruct = ims_msgStructAlloc()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) sprintf( display_string,
			"Can't begin IMS/DADS operation\n Too many processes running?" );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		return (NULL);
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

	return( imsMsgStruct );
}


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
Function:       aps_darQuery

Description:    does all the IMS setup processing, calls ims_darQuery,
				then does all the IMS close processing

Parameters:     

Returns:        IMS_NOROWS	- IMS didn't find any rows matching the query
				>= IMS_OK	- IMS found rows matching the query
				all others	- an erroneous condition occurred

Creator:        Quentin Sun

Creation Date:  01/30/96

Notes:		
==============================================================================*/
int
aps_darQuery(
	char* dar_status, 
	char* dar_startTime , 
	char* dar_endTime,
	IMS_DAR_LIST *resultDarList, 
   	IMS_MSG_STRUCT *msgDesc)
{
    int status;

	/*
	** Set up the query structure.
	*/

	if ((status = set_ims_query_struct( msgDesc )) < IMS_OK)
	{
		return( status );	/* error message is in IMS message struct */
	}

	imsdb_query->msgDesc    = msgDesc;
	imsdb_query->retStatus  = IMS_OK;
	imsdb_query->retPtr     = (char *) resultDarList;

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
    ** Perform the query.
    */

    if ((status = ims_darQuery(
		imsdb_query, dar_status, dar_startTime, dar_endTime )) < IMS_OK)
    {
		if (imsdb_query->retStatus == IMS_NOROWS)
			status = IMS_NOROWS;
		else
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

} /* aps_darQuery */


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
