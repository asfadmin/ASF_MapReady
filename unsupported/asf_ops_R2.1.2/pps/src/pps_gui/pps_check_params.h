/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _PPS_CHECK_PARAMS_H_INCLUDED
#define _PPS_CHECK_PARAMS_H_INCLUDED

#pragma ident	"@(#)pps_check_params.h	1.5  10/31/97"

#include "PPShdr.h"

/* structure to store data retrieved from PPS db needed to perform IMS 
 * queries */
typedef struct Job_Rec
{
	char	start_time[TIME_STRLEN+1];
	char	end_time[TIME_STRLEN+1];
	char	center_time[TIME_STRLEN+1];
   	char 	platform[PLATFORM_STRLEN+1];  
	char 	sensor[SENSOR_STRLEN+1];
	int	rev;
	int	rev_is_null;
	char 	mode[MODE_STRLEN+1];
	char 	quicklook_flag[LOGICAL_STRLEN+1];
	int	sequence;
	int	sequence_is_null;
	char 	media_id[MEDIA_ID_STRLEN+1];
	char	auto_schedulable[LOGICAL_STRLEN+1];
	char	job_comment[COMMENT_STRLEN+1];
} Job_Rec;

/* IMS params availability indication */
typedef struct Params_Avail
{
        char	GHA_avail[LOGICAL_STRLEN+1];
        char	TCE_avail[LOGICAL_STRLEN+1];
        char	SV_avail[LOGICAL_STRLEN+1];
        char    Scan_File_avail[LOGICAL_STRLEN+1];
        char    Params_File_avail[LOGICAL_STRLEN+1];
	char	media_id[MEDIA_ID_STRLEN+1];
} Params_Avail;

/*************************************************************************
 * The "check_params" routine queries IMS for various processing
 * parameters. The caller can examine "avail_indicators" when 
 * ER_JOB_IS_NOW_READY or ER_JOB_IS_STILL_PENDING status code is returned.
 * This routine returns various error status code when it fails to access
 * either the IMS or PPS database, in which case, the caller should take
 * appropriate action.  
 *************************************************************************/  
int
check_params (int		  order_id,
	      int		  item_id,
	      int                 job_id,
              char                *job_type,
	      char		  *previous_job_state,
              struct Params_Avail *avail_indicators); 

#endif /*_PPS_CHECK_PARAMS_H_INCLUDED*/
