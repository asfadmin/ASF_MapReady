/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	match_L1request.c

Description:
	This module contains the function used for determining
	if a job with the same processing paramters already 
	exists and if so returns the job_id of the job 

External Functions:
	match_L1request
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)match_L1request.c	1.3    10/31/97";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sybfront.h>
#include <sybdb.h>
#include "db_sybint.h"
#include "db_l1_order.h"
#include "PPSdefs.h"
#include "PPShdr.h"

extern COLUMN_DEFS	l1_procparms_columns_[];

int  match_L1request(DBPROCESS *, IMS_L1PReq_Record *, int *) ;


/*==============================================================================
Function:	match_L1request

Description:	
	Search the L1 Processing Parameters table to see if a job
	with the same processing paramters already exists.

Parameters: 	
	dbproc		dbproc server
	rec		L1 product request record
	job_id		job_id of the job (if found)
Returns:	
	TRUE		a matching job exits
	FALSE		does not exist
Creator:		Nadia Adhami
Creation Date:		5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int  match_L1request(DBPROCESS *dbproc, IMS_L1PReq_Record *rec, int *job_id)
#else
int  match_L1request(dbproc, rec, job_id)
	DBPROCESS 		*dbproc;
	IMS_L1PReq_Record       *rec;
	int 			*job_id;
#endif
{
	char	buf[1024];
	int	nrecs = 0;

	/* search L1_procparms table for a job with the *
	 * same processing paramters			*/

	sprintf(buf,
	"sp_match_l1_order \"%s\",\"%s\",\"%s\",%d,\"%s\",\"%s\",%d,%f,%d,%d, "
		"\"%s\",\"%s\",%d,\"%s\",%f,%f,%f,%d,\"%s\",%f,%f,\"%s\"", 
		rec->quicklook_flag,
		rec->platform, rec->sensor, rec->rev, rec->mode,
		rec->product_type, rec->datatake_seq,
		rec->pixel_spacing, rec->frame_id, rec->subframe_id,
		rec->output_format, rec->projection,
		rec->processing_gain, rec->deskew, rec->avg_terrain_ht,
		rec->ps_reference_lat, rec->ps_reference_lon, rec->utm_zone,
		rec->terrain_correction,
		rec->lambert_latitude_n, 
		rec->lambert_latitude_s,
		rec->compensation_flag);

        /* execute stored procedure to find a matching L1 order */
        db_exec_cmd (dbproc, buf, job_id, &nrecs);
 
	if (*job_id > 0)
	{
#ifdef DEBUG
        	printf("match_L1request() -> jobid   %d\n", *job_id);
#endif
		return (TRUE);
	}
	else
		return (FALSE);

} /* match_L1request */

/* End of File */
