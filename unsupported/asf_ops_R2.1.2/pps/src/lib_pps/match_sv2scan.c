/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	match_sv2scan.c

Description:
	This module contains the routines used for processing  IMS messages. 

External Functions:
	match_svec_2_scan
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)match_sv2scan.c	1.1    11/21/96";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "PPSdefs.h"
#include "db_sybint.h"
#include "PPSerr.h"

/*==============================================================================
Function:	
	int match_svec_2_scan(svec, scan)

Description:	
	This function checks if a state vector matches a IMS Scan Request.

Parameters:
        IMS_SVecAvail_Record    *svrec;
        IMS_ScanReq_Record      *scan;

Returns:
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int match_svec_2_scan(IMS_SVecAvail_Record *svrec, IMS_ScanReq_Record *scan)
#else
int match_svec_2_scan(svrec, scan)
        IMS_SVecAvail_Record    *svrec;
        IMS_ScanReq_Record      *scan;
#endif
{

	int 		res = FALSE;
	Time_Record 	tmp_timerec ;

	/* case 1 :
	 * svrec.start_time <= scan.start_time <= svrec.end_time 
	 */ 
	timerec_range (scan->start_time,
		svrec->start_time, svrec->end_time, TRUE, &res );
	if (res == TRUE)
		return (TRUE);

	/* case 2 :
	 * svrec.end_time >= scan.start_time - 100 minutes 
	 */ 
	tmp_timerec = scan->start_time;
	/* subtract 100 minutes form scan's start time */
	timerec_subtract (tmp_timerec, 100, unit_minute);
	/* compare the new value to the state vector start time */
	timerec_cmp(svrec->start_time, tmp_timerec, &res) ;

	if ((res == greaterthan) || (res == equal))
		return TRUE;
	else
		return FALSE;


} /* process_IMS_DubReq */


/* End of File */
