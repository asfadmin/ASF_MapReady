/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ims_int.c

Description:
	This module contains the routines which interface with IMS API and
	are used for query from IMS or send status message to IMS.

External Functions:
	ims_db_connect		
	ims_db_disconnect
	wrap_ims_ghaRangeQuery
	wrap_ims_svQuery
	wrap_ims_tceQuery
	wrap_ims_calParamQuery
	wrap_ims_scanQuery
	wrap_ims_orderStatus
	wrap_ims_frameStatus
	wrap_ims_frameQuery
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)ims_int.c	1.6    10/31/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#ifndef NO_THREAD
#include <pthread.h>
#endif

#include "defs.h"
#include "PPSextern.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern PPSConfigStruct	PPSConfigs[];

#ifndef NO_THREAD
extern pthread_mutex_t  g_mutex_for_odllib;
#endif

/*==============================================================================
Function:       ims_db_connect
Description:    creates and initializes the connection structure used by IMS to 		perform IMS queries.
==============================================================================*/
int ims_db_connect(IMS_CMN_QUERY **ims_query)
{
        int 		status;
        IMS_MSG_STRUCT  *msgDesc;
	IMS_CMN_QUERY	*aux_query;
	char		logmsg[MAX_SYSLOG_MSGLEN+1];
	char		program_substring[30];
	char		*str;
 
        /* Allocate and initialize the message facility.  */
        if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *)NULL)
                return (ER_IMS_MALLOC);
 
        /* Inititialize the message facility options. */
        (void) ims_msgSubSystem (msgDesc, "IMS");
        (void) ims_msgBanner (msgDesc, ProgName, IMS_MSG_ALLBANNER);
        (void) ims_msgStderrFlag (msgDesc, IMS_ON);
        (void) ims_msgQueueFlag (msgDesc, IMS_OFF);
 
	/* Allocate and initialize the query facility.  */
	if ((aux_query = (IMS_CMN_QUERY *)malloc(sizeof(IMS_CMN_QUERY)))
                        == NULL)
	{
		sprintf (logmsg,"Memory allocation for IMS_CMN_QUERY failed.");
		fprintf (stderr, "%s\n", logmsg);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		(void)ims_msgStructFree(msgDesc);
		return (ER_IMS_MALLOC);
	}
 
        /* populate the query data structure */
	/*--------------------------------------------*/
	/* strip the path, and truncate the program   */
	/* name to 30 chars                           */
	/*--------------------------------------------*/
	if ((str=strrchr(ProgName, '/')) == NULL)
		str = ProgName;
	else
		str++;
	(void)strncpy(program_substring, str, 29);
	program_substring[29] = '\0';

	strcpy (aux_query->program  , program_substring);
        strcpy (aux_query->username , PPSConfigs[IMS_USERID].value);
        strcpy (aux_query->password , PPSConfigs[IMS_PASSWD].value);
        strcpy (aux_query->server   , PPSConfigs[IMS_SERVER].value);
        strcpy (aux_query->database , PPSConfigs[IMS_DBNAME].value);
        aux_query->msgDesc        = msgDesc;
        aux_query->retPtr         = NULL;
        aux_query->qDesc          = NULL;

        /* open sybase connection for IMS */
        status = ims_openQueryConnection(aux_query);
        if (status < IMS_OK)
	{
                return (ER_IMS_OPEN_DB_CONNECT);
	}
        else
	{
		*ims_query = aux_query;
                return (ER_NO_ERROR);
	}
}

/*==============================================================================
Function:       ims_db_disconnect
Description:    free the connection structure used by IMS to
                perform IMS queries.
==============================================================================*/
int ims_db_disconnect(IMS_CMN_QUERY *ims_query)
{
        if (ims_query != NULL)
                (void)ims_closeQueryConnection (ims_query);

	return(ER_NO_ERROR);
}

/******************************************************************************
Function:	ims_db_reconnect
Description:    This function is executed when an IMS API query failed with
                IMS_NOCONNECT error code which might be caused by a stale
                connection.  The existing connection is dropped, and a new
                connection is established.  If IMS still returns IMS_NOCONNECT
                then it is mostly likely that IMS SQL Server is down. 
*******************************************************************************/
int ims_db_reconnect(char	   *API_name,
	             IMS_CMN_QUERY **ims_query,
		     int	   *retry,
		     int	   *retry_cnt)
{
        char  reconnect_msg[MAX_SYSLOG_MSGLEN+1];
	int   ret_code;

        sprintf(reconnect_msg,"Attempt to reconnect to IMS for %s", API_name);
        pps_logMsg(ProgName, PPS_INFO, reconnect_msg);
        
	/* Drop the existing connection */
        (void) ims_db_disconnect(*ims_query);
        
	/* Make a new connection */
        if (ims_db_connect(ims_query) != ER_NO_ERROR)
        {
                pps_logMsg(ProgName, PPS_ERROR,
                           "Attempt to reconnect to IMS server FAILED");
                ret_code = IMS_NOCONNECT;
                *retry = FALSE;
        }
        else
        {
		pps_logMsg(ProgName, PPS_INFO,
			   "Attempt to reconnect to IMS server SUCCEEDED");
		ret_code = ER_NO_ERROR;
                *retry = TRUE;
                *retry_cnt = 0;
        }
	return (ret_code);
}
 

/*==============================================================================
Function:       void get_platform_name(char *platform, char *platform_name)
Description:    get the platform name 
		E1 -> ERS-1 , etc.
Parameters:	platform
Returns:	platform name (long form)
Creator:        Nadia Adhami
Creation Date:  Tue Oct 17 15:38:05 PDT 1995
Notes:
==============================================================================*/

void get_platform_name( char *platform , char *platform_name)
{
	if (! strcmp(platform, "E1"))
		strcpy ( platform_name, "ERS-1" );
	if (! strcmp(platform, "E2"))
		strcpy ( platform_name, "ERS-2" );
	if (! strcmp(platform, "J1"))
		strcpy ( platform_name, "JERS-1" );
	if (! strcmp(platform, "R1"))
		strcpy ( platform_name, "RADARSAT-1" );
	if (! strcmp(platform, "A1"))
		strcpy ( platform_name, "ADEOS-1" );
}

/*==============================================================================
Function:	int wrap_ims_ghaRangeQuery(IMS_CMN_QUERY **ims_query, 
			char *start_time, char *end_time, 
			struct GHA_Correction  *pps_gha)
Description:	call IMS GHA Query API	
Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:38:05 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_ghaRangeQuery(
	IMS_CMN_QUERY **ims_query,
	char *start_time, char *end_time, 
	struct GHA_Correction  *pps_gha)
#else
int wrap_ims_ghaRangeQuery(ims_query, start_time, end_time, pps_gha);
	IMS_CMN_QUERY	**ims_query;
	char 		*start_time;
	char 		*end_time;
	struct GHA_Correction	*pps_gha;
#endif
{
	int 		ret_code;
	int 		status;
	int		retry = TRUE; 	/* retry flag */
	int		retry_cnt = 0;	/* retry counter */
	char 		*ptr;
	IMS_GHA_STRUCT	gha;
        char            adjusted_start_time[TIME_STRING_LEN+1];
        char            adjusted_end_time[TIME_STRING_LEN+1];
        time_t          seconds;
        unsigned short  milliseconds;
        char            rc;
        char            logmsg[MAX_SYSLOG_MSGLEN];
 
        /* The following processing is needed to get the correct
           GHA from IMS */
        strcpy(adjusted_end_time, start_time);
        rc = odl_timestring_to_seconds
             (start_time, &seconds, &milliseconds);
        if ( ! rc)
                return (ER_BAD_ODL_TIME_FORMAT);

        seconds -= GHA_CYCLE_TIME ;
        rc = seconds_to_odl_timestring
              (adjusted_start_time, seconds, milliseconds);
        if ( ! rc)
                  return (ER_BAD_ODL_TIME_FORMAT);

	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;

	        /* set the return ptr in the IMS query structure */
	        (*ims_query)->retPtr         = (void *) &gha;

		status = ims_ghaRangeQuery(*ims_query, 
			adjusted_start_time, adjusted_end_time);
		if (status == IMS_OK)
		{
			/* no need to retry again */
			retry = FALSE;
			/* convert the angle from ascii to float */
			sscanf (gha.angle, "%f", &pps_gha->angle);
#ifdef DEBUG
			printf("ims_ghaRangeQuery (%s,%s) %s\n", 
				start_time, end_time, gha.angle);
#endif
			convert_date_string (gha.date , &pps_gha->time);

			return(ER_NO_ERROR);
		}
		/* status != IMS_OK */
		/* check Sybase errors */
		switch ((*ims_query)->retStatus)
		{
		case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                "ims_ghaRangeQuery error - Deadlock");
			ret_code = ER_IMS_GHA_QUERY_FAIL;
			break;
		case IMS_NOROWS:
			retry = FALSE;
			ret_code = ER_IMS_GHA_QUERY_NODATA;
			break;
		case IMS_NOCONNECT:
			if (ims_db_reconnect("ims_ghaRangeQuery",
			   ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_GHA_QUERY_FAIL;
			break;
		case IMS_QUERYFAIL:
		default:
			retry = FALSE;
			ret_code = ER_IMS_GHA_QUERY_FAIL;
			break;
		}
	} /* end ofretry logic */

	return (ret_code);
}


/*==============================================================================
Description:	call IMS calParam Query API	
Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:38:05 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_calParamQuery(
	IMS_CMN_QUERY **ims_query,
	char *platform, char *mode, char *centerTime,
	char *cal_params_file, char *cal_params_file2)
#else
int wrap_ims_calParamQuery(ims_query, platform, mode, centerTime, 
			   cal_params_file, cal_params_file2);
	IMS_CMN_QUERY	**ims_query;
	char 		*platform;
	char 		*mode;
	char 		*centerTime;
	char 		*cal_params_file;
	char            *cal_params_file2;
#endif
{
	int 		ret_code;
	int 		status;
	int		retry = TRUE; 	/* retry flag */
	int		retry_cnt = 0;	/* retry counter */
	char 		platform_name[10];
	IMS_CAL_PARAM_STRUCT *calParamsList, *calParamsPtr;

	get_platform_name( platform, platform_name );
	cal_params_file[0] = '\0';
	cal_params_file2[0] = '\0';

#ifdef DEBUG
	printf("ims_calParamQuery(%s,%s,%s)\n", 
				platform_name, mode, centerTime);
#endif
	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
		(*ims_query)->retPtr = NULL;
		status = ims_calParamQuery(*ims_query, platform_name,mode,centerTime);
		if (status == IMS_OK)
		{
			/* no need to retry again */
			retry = FALSE;

			/* get data contained in the return list -
			   we're only interested in getting at most 2 files */
	
			calParamsPtr = (IMS_CAL_PARAM_STRUCT *)(*ims_query)->retPtr;
			calParamsList = calParamsPtr;

			PPS_STRNCPY(cal_params_file, calParamsPtr->file_name,
                                CAL_PARAMS_FILE_STRLEN);
			if ((calParamsPtr = calParamsPtr->next) !=
				(IMS_CAL_PARAM_STRUCT *) NULL)
			{
                        	PPS_STRNCPY(cal_params_file2, 
					calParamsPtr->file_name,
                                	CAL_PARAMS_FILE_STRLEN);

			}

			/* free the return list */
			(void)ims_calParamFree(calParamsList);
				

#ifdef DEBUG
			printf("ims_calParamQuery(%s,%s,%s) file1='%s', file2= '%s'\n",
				platform_name, mode, centerTime, 
				cal_params_file, cal_params_file2);
#endif

			return(ER_NO_ERROR);
		}
		/* status != IMS_OK */
		/* check Sybase errors */
		switch ((*ims_query)->retStatus)
		{
		case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
			             "ims_calParam error - Deadlock");
			ret_code = ER_IMS_CAL_QUERY_FAIL;
			break;
		case IMS_NOROWS:
			retry = FALSE;
			ret_code = ER_IMS_CAL_QUERY_NODATA;
			break;
		case IMS_NOCONNECT:
			if (ims_db_reconnect("ims_calParam",
                           ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_CAL_QUERY_FAIL;
			break;
		case IMS_QUERYFAIL:
		default:
			retry = FALSE;
			ret_code = ER_IMS_CAL_QUERY_FAIL;
			break;
		}
	} /* end ofretry logic */

	return (ret_code);
}

/*==============================================================================
Function:	int wrap_ims_scanQuery(IMS_CMN_QUERY **ims_query, 
			char *platform, 
			long rev,
			short sequence,
			char *media_id, 
			char *mode)
Description:	call IMS scan Results Query API	
Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:38:05 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_scanQuery(
	IMS_CMN_QUERY **ims_query,
	char *platform, int rev, int sequence,
        char *media_id, char *mode, char *scan_results_file)
#else
int wrap_ims_scanQuery(ims_query, platform, rev, sequence, media_id, mode,
		scan_results_file);
	IMS_CMN_QUERY	**ims_query;
	char 		*platform;
	long 		*rev;
	short 		*sequence;
	char 		*media_id;
	char 		*mode;
	char 		*scan_results_file;
#endif
{
	int 		ret_code;
	int 		status;
	int		retry = TRUE; 	/* retry flag */
	int		retry_cnt = 0;	/* retry counter */
	char		buffer[256];
	char 		platform_name[10];
	IMS_SCAN_STRUCT retbuf;

	/* convert to IMS platform : E1 -> ERS1, etc */
	get_platform_name( platform, platform_name );

	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
	        /* initliaze the return buffer */
	        buffer[0] = '\0';
	        /* set the return buffer pointer */
	        (*ims_query)->retPtr         = (char *) &retbuf;

		status = ims_scanQuery(*ims_query, platform_name, rev,
			sequence, media_id, mode);
		if (status == IMS_OK)
		{
			/* no need to retry again */
			retry = FALSE;

			/* data is contained in the return buffer */
			PPS_STRNCPY(scan_results_file, retbuf.scan_results_file,
				SCAN_RESULTS_FILE_STRLEN);
#ifdef DEBUG
			printf("ims_scanQuery(%s,%d,%d,%s,%s) %s\n",
				platform_name, rev, sequence, media_id,
				mode, scan_results_file);
#endif

			return(ER_NO_ERROR);
		}
		/* status != IMS_OK */
		/* check Sybase errors */
		switch ((*ims_query)->retStatus)
		{
		case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
			        "ims_scanQuery error - Deadlock");
			ret_code = ER_IMS_SCAN_QUERY_FAIL;
			break;
		case IMS_NOROWS:
			retry = FALSE;
			ret_code = ER_IMS_SCAN_QUERY_NODATA;
			break;
		case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_scanQuery",
                           ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_SCAN_QUERY_FAIL;
			break;
		case IMS_QUERYFAIL:
		default:
			retry = FALSE;
			ret_code = ER_IMS_SCAN_QUERY_FAIL;
			break;
		}
	} /* end ofretry logic */

	return (ret_code);
}


/*==============================================================================
Function:	int wrap_ims_tceQuery(IMS_CMN_QUERY **ims_query, 
				  char *start_time, char *end_time, 
				  char *platform, struct Time_Correlation *tc)
Description:	call IMS TCE Query API	
Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:38:05 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_tceQuery(
	IMS_CMN_QUERY **ims_query,
	char *start_time, char *end_time, 
	char *platform, struct Time_Correlation *tc)
#else
int wrap_ims_tceQuery(ims_query, start_time, end_time, platform, tc)
	IMS_CMN_QUERY	**ims_query,
	char		*start_time;
	char 		*end_time;
	char   		*platform;
	struct 		Time_Correlation *tc;
#endif
{
	int		status;
	int		retry = TRUE;
	int		retry_cnt = 0;
	int		ret_code;
	char		*ptr;
	char 		platform_name[10];
        char            adjusted_start_time[TIME_STRING_LEN+1];
        char            adjusted_end_time[TIME_STRING_LEN+1];
        time_t          seconds;
        unsigned short  milliseconds;
        char            rc;
        char            logmsg[MAX_SYSLOG_MSGLEN];
	IMS_TCE_STRUCT  retbuf;

	get_platform_name( platform, platform_name );

        /* The following processing is needed to get the correct
           TCE from IMS */
	strcpy(adjusted_end_time, start_time);
        rc = odl_timestring_to_seconds
             (start_time, &seconds, &milliseconds);
        if ( ! rc)
                return (ER_BAD_ODL_TIME_FORMAT);

	seconds -= TCE_CYCLE_TIME;
        rc = seconds_to_odl_timestring
              (adjusted_start_time, seconds, milliseconds);
        if ( ! rc)
               	  return (ER_BAD_ODL_TIME_FORMAT); 

	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
#ifdef DEBUG
		printf("ims_tceQuery with adjusted start-end time(%s,%s,%s)\n",
			platform_name,adjusted_start_time,adjusted_end_time);
#endif
		retry_cnt += 1;

	        /* set the return ptr in the IMS query structure */
	        (*ims_query)->retPtr         = (void *) &retbuf;

		status = ims_tceQuery(*ims_query,platform_name,
			adjusted_start_time,adjusted_end_time);
		if (status == IMS_OK)
		{
			unsigned int sat_time;
        		sscanf (retbuf.rev,      "%d", &tc->rev);
        		sscanf (retbuf.sat_time, "%u", &sat_time);
			/* unsigned integer was sent in, but
			  sybase can only store int, so keep it as int */
			tc->sat_time = (int) sat_time;
        		sscanf (retbuf.clk_cycle,"%d", &tc->clock_cycle);

#ifdef DEBUG
			printf("ims_tceQuery(%s,%s,%s) rev=%s sattime=%s clkcyc=%s\n",
				platform_name,start_time,end_time,
				retbuf.rev,retbuf.sat_time,retbuf.clk_cycle);
#endif
			convert_date_string (retbuf.date , &tc->time);
			return (ER_NO_ERROR);
		}

                /* status != IMS_OK */
                /* check Sybase errors */
                switch ((*ims_query)->retStatus)
                {
                case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                "ims_tceQuery error - Deadlock");
                        ret_code = ER_IMS_TCE_QUERY_FAIL;
			break;
                case IMS_NOROWS:
                        retry = FALSE;
                        ret_code = ER_IMS_TCE_QUERY_NODATA;
			break;
                case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_tceQuery",
                            ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_TCE_QUERY_FAIL;
			break;
                case IMS_QUERYFAIL:
                default:
                        retry = FALSE;
                        ret_code = ER_IMS_TCE_QUERY_FAIL;
			break;
                }
	} /* end of retry logic */

	return (ret_code);

} /* ims_tceQuery */




/*==============================================================================
Function:	int wrap_ims_svQuery(IMS_CMN_QUERY **ims_query, char *platform,
				   char *precision, char *start_time, 
				   char *end_time, struct State_Vector *sv)
Description:	call IMS State Vector Query API	
Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:38:05 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_svQuery(IMS_CMN_QUERY **ims_query,
	char *platform,
	char *precision,
	char *start_time, char *end_time, 
	struct State_Vector *sv)
#else
int wrap_ims_svQuery(ims_query, platform, precision,
    start_time, end_time, sv)
	IMS_CMN_QUERY **ims_query;
	char	*platform;
	char	*precision;
	char 	*start_time;
	char 	*end_time;
	struct	State_Vector *sv;
#endif
{
	int		status;
	int		retry = TRUE;
	int		retry_cnt = 0;
	int		ret_code;
	char		*ptr;
	char 		platform_name[10];
	IMS_SV_STRUCT   retbuf;
	char		adjusted_start_time[TIME_STRING_LEN+1];
	char		adjusted_end_time[TIME_STRING_LEN+1];
	time_t		seconds;
	unsigned short	milliseconds;
	char		rc;
	char    	logmsg[MAX_SYSLOG_MSGLEN];

	get_platform_name( platform, platform_name );

        /* The following processing is needed to get the correct
           state vectors from IMS */
	strcpy(adjusted_end_time, start_time);
        rc = odl_timestring_to_seconds
             (start_time, &seconds, &milliseconds);
        if ( ! rc)
                return (ER_BAD_ODL_TIME_FORMAT);

	if (strcmp(platform,"E1") == 0)
		seconds -= E1_ORBIT_TIME;
	else if (strcmp(platform,"E2") == 0) 
                seconds -= E2_ORBIT_TIME;	
        else if (strcmp(platform,"J1") == 0)   
                seconds -= J1_ORBIT_TIME;  
        else if (strcmp(platform,"R1") == 0)   
                seconds -= R1_ORBIT_TIME;  
        else
	{ 
		(void)sprintf(logmsg,
                   "Unknown Platform [%s] encountered in wrap_ims_svQuery",
                   platform);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		seconds -= DEFAULT_ORBIT_TIME;
	}
 
        rc = seconds_to_odl_timestring
              (adjusted_start_time, seconds, milliseconds);
        if ( ! rc)
               	  return (ER_BAD_ODL_TIME_FORMAT); 
#ifdef DEBUG
         printf("ims_svQuery with adjusted start-end time(%s,%c,%s,%s)\n",
                platform_name,precision[0], 
		adjusted_start_time, adjusted_end_time);
#endif

	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
	        /* set the return ptr in the IMS query structure */
	        (*ims_query)->retPtr         = (void *) &retbuf;

		status = ims_svQuery(*ims_query, platform_name, precision[0], 
			adjusted_start_time, adjusted_end_time, IMS_FALSE);
		if (status == IMS_OK)
		{
			strcpy (sv->precision, precision);
        		sscanf (retbuf.rev, "%d", &sv->rev);
        		sscanf (retbuf.x_pos, "%f", &sv->x_pos);
        		sscanf (retbuf.y_pos, "%f", &sv->y_pos);
        		sscanf (retbuf.z_pos, "%f", &sv->z_pos);
        		sscanf (retbuf.x_vel, "%f", &sv->x_vel);
        		sscanf (retbuf.y_vel, "%f", &sv->y_vel);
        		sscanf (retbuf.z_vel, "%f", &sv->z_vel);
			strcpy (sv->coord_sys, retbuf.coord_sys);
			convert_date_string (retbuf.date , &sv->time);
			return (ER_NO_ERROR);
		}
                /* status != IMS_OK */
                /* check Sybase errors */
                switch ((*ims_query)->retStatus)
                {
                case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                   "ims_svQuery error - Deadlock");
                        ret_code = ER_IMS_SV_QUERY_FAIL;
			break;
                case IMS_NOROWS:
                        retry = FALSE;
                        ret_code = ER_IMS_SV_QUERY_NODATA;
			break;
                case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_svQuery",
                            ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_SV_QUERY_FAIL;
			break;
                case IMS_QUERYFAIL:
                default:
                        retry = FALSE;
                        ret_code = ER_IMS_SV_QUERY_FAIL;
			break;
		}
	} /* end of retry logic */
	return (ret_code);

} /* ims_svQuery */


/*==============================================================================
Function:	wrap_ims_orderStatus

Description:	
	send status message to IMS

Parameters: 	
	ims_query		handle to ims database
	buffer			containing ODL formatted message
Returns:	
	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int  	wrap_ims_orderStatus(IMS_CMN_QUERY **ims_query, char *buffer)
#else
int  	wrap_ims_orderStatus(ims_query, buffer);
	IMS_CMN_QUERY 		**ims_query;
        char 			*buffer;
#endif
{
	int		status = IMS_OK;
	int		retry = TRUE;
	int		retry_cnt = 0;
	int		ret_code;

#ifdef ODL_DEBUG
        fprintf(stdout, "PPS -> IMS (order status):\n");
        fprintf(stdout, buffer);
#endif

	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
	        /* set the return ptr in the IMS query structure */
	        (*ims_query)->retPtr         = NULL;
#ifndef NO_THREAD
		/* Since this call is not re-entrant, we need to mutex lock */
		pthread_mutex_lock(& g_mutex_for_odllib);
#endif
		status = ims_orderStatus(*ims_query, buffer);

#ifndef NO_THREAD
		pthread_mutex_unlock(& g_mutex_for_odllib);
#endif
		
		if (status == IMS_OK)
			return (ER_NO_ERROR);

                /* status != IMS_OK */
                /* check Sybase errors */
                switch ((*ims_query)->retStatus)
                {
                case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                  "ims_orderStatus error - Deadlock");
                        ret_code = ER_IMS_SEND_STATUS_FAIL;
			break;
                case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_orderStatus",
                            ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_SEND_STATUS_FAIL;
			break;
		case IMS_NOROWS:
			pps_logMsg(ProgName, PPS_ERROR,
                                  "Order not found in IMS database");
                        ret_code = ER_IMS_ORDER_NOT_FOUND;
			retry = FALSE;
                        break;

                default:
		/* IMS_QUERYFAIL: */
                        retry = FALSE;
                        ret_code = ER_IMS_SEND_STATUS_FAIL;
			break;
		}
	} /* end of retry logic */
	return (ret_code);

} /* wrap_ims_orderStatus */


/*==============================================================================
Function:	int wrap_ims_frameQuery(IMS_CMN_QUERY **ims_query, 
			char *platform, int rev, int sequence, 
			char *media_id, char *mode,
			int *frame_count) 
Description:	call IMS Frame Info Query API	
Parameters:	
Returns:	
Creator:	Norbert Piega
Creation Date:	12/05/95
Notes:		
==============================================================================*/
#ifdef __STDC__
int wrap_ims_frameQuery(IMS_CMN_QUERY **ims_query,
	char *platform,
	int rev,
	int sequence,
	char *media_id,
	char *mode,
	char *frame_mode,
	int *frame_count)
#else
int wrap_ims_frameQuery(ims_query, platform, rev, sequence, media_id, mode, frame_mode, frame_count)
	IMS_CMN_QUERY **ims_query;
	char	*platform;
	int	rev;
	int 	sequence;
	char 	*media_id;
	char 	*mode;
	char	*frame_mode;
	int	*frame_count;
#endif
{
	int		status;
	int		retry = TRUE;
	int		retry_cnt = 0;
	int		ret_code;
	char		platform_name[10];
	IMS_FRAME_STRUCT *listptr;

	get_platform_name( platform, platform_name );

#ifdef DEBUG
	printf("ims_frameQuery(rev=%d,seq=%d,platform_name=%s,media_id=%s,mode=%s,frame_mode=%s)\n",
		rev, sequence, platform_name, media_id, mode,frame_mode);
#endif
	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
		(*ims_query)->retPtr = NULL;
		status = ims_frameQuery(*ims_query, platform_name, rev,
					sequence, media_id, mode, frame_mode);
		if (status == IMS_OK)
		{
#ifdef DEBUG
			printf("ims_frameQuery-> ");
#endif
			*frame_count = 0;
			listptr = (IMS_FRAME_STRUCT *)(*ims_query)->retPtr;
			while (listptr != (IMS_FRAME_STRUCT *)NULL)
			{
#ifdef DEBUG
				printf("%d ", listptr->frame_id);
#endif
				(*frame_count)++ ; 
				listptr = listptr->next;
			}
#ifdef DEBUG
			printf("\n");
			printf("ims_frameQuery(frame_count=%d)\n",*frame_count);
#endif
			return(ER_NO_ERROR);
		}

                /* status != IMS_OK */
                /* check Sybase errors */
                switch ((*ims_query)->retStatus)
                {
                case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                    "ims_frameQuery error - Deadlock");
                        ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
                case IMS_NOROWS:
                        retry = FALSE;
                        ret_code = ER_IMS_FRAME_QUERY_NODATA;
			break;
                case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_frameQuery",
                            ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
                case IMS_QUERYFAIL:
                default:
                        retry = FALSE;
                        ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
		}
	} /* end of retry logic */
	return (ret_code);

} /* ims_frameQuery */

/*==============================================================================
Function:	int wrap_ims_frameStatus(IMS_CMN_QUERY **ims_query, 
			int order_id, int item_id,
			IMS_FRAME_SOURCE_MEDIA *)
Description:	call IMS Frame Status Query API	
Creator:	Thuy Tran	
Creation Date: 	08/15/97	
Notes:		
==============================================================================*/
int wrap_ims_frameStatus(IMS_CMN_QUERY **ims_query,
	int order_id,
	int item_id,
	IMS_FRAME_SOURCE_MEDIA_STRUCT *frame_status)
{
	int		status;
	int		retry = TRUE;
	int		retry_cnt = 0;
	int		ret_code;

#ifdef DEBUG
	printf("ims_frameStatus(order=%d,item=%d\n", order_id, item_id);
#endif
	/* retry logic */
	while (retry && (retry_cnt < IMS_MAX_RETRY))
	{
		retry_cnt += 1;
		(*ims_query)->retPtr = (void *) frame_status;
		status = ims_frameStatus(*ims_query, order_id, item_id); 
		if (status == IMS_OK)
		{
			return(ER_NO_ERROR);
		}

                /* status != IMS_OK */
                /* check Sybase errors */
                switch ((*ims_query)->retStatus)
                {
                case IMS_DEADLOCK:
			pps_logMsg(ProgName, PPS_ERROR,
                                    "ims_frameStatus error - Deadlock");
                        ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
                case IMS_NOROWS:
                        retry = FALSE;
                        ret_code = ER_IMS_FRAME_QUERY_NODATA;
			break;
                case IMS_NOCONNECT:
                        if (ims_db_reconnect("ims_frameStatus",
                            ims_query,&retry,&retry_cnt) == IMS_NOCONNECT)
				ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
                case IMS_QUERYFAIL:
                default:
                        retry = FALSE;
                        ret_code = ER_IMS_FRAME_QUERY_FAIL;
			break;
		}
	} /* end of retry logic */
	return (ret_code);

} /* ims_frameStatus */
